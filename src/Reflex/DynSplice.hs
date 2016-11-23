{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Reflex.DynSplice where

-------------------------------------------------------------------------------
import           Control.Applicative    (Alternative, liftA2, (<|>))
import           Control.Lens
import           Control.Monad          (join)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bifunctor         (second)
import qualified Data.Binary.Builder    as B
import           Data.Bool              (bool)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Either            (either, isLeft)
import           Data.Foldable          (foldl')
import qualified Data.Map               as M
import qualified Data.Map.Syntax        as M
import           Data.Maybe             (catMaybes, fromMaybe, maybe)
import           Data.Monoid
import           Data.Set               ((\\))
import qualified Data.Set               as S
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Heist                  as H
import qualified Heist.Interpreted      as HI
import           Reflex.Dom
import           Text.Read              (readMaybe)
import qualified Text.XmlHtml           as X

-------------------------------------------------------------------------------
-- Requests for dynamic splices embedded in a template
data UiSpliceHole = UiSpliceText
                  | UiSpliceDouble
                  | UiSpliceDropdown [(T.Text,T.Text)]
                  | UiSpliceUntyped
                    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- As we walk the DOM searching for SpliceHoles, some splices may be named
-- in multiple places, sometimes with annotations and other times without.
-- This instance resolves conflicts by defaulting to an untyped splicehole
-- but overwriting with the the type of whichever spliceHole is found first
instance Monoid UiSpliceHole where
  mempty = UiSpliceUntyped
  UiSpliceUntyped `mappend` x = x
  x               `mappend` _ = x


-------------------------------------------------------------------------------
-- Traverse all templates looking for splice requests
collectHoles
  :: (T.Text -> [(T.Text,T.Text)] -> Maybe UiSpliceHole)
  -- ^ Parse a splice hole and tag name from a tag name and attribute list
  -> [H.Template]
  -> M.Map T.Text UiSpliceHole
collectHoles parseTag ts =
  concatUiHoles (map accumTemplate ts)
  where
    accumTemplate :: H.Template -> M.Map T.Text UiSpliceHole
    accumTemplate t = concatUiHoles (spliceHoles <$> t)

    addHole :: UiSpliceHole -> UiSpliceHole -> UiSpliceHole
    addHole oldV newV = oldV <> newV

    concatUiHoles :: [M.Map T.Text UiSpliceHole] -> M.Map T.Text UiSpliceHole
    concatUiHoles = M.unionsWith (<>)

    spliceHoles :: X.Node -> M.Map T.Text UiSpliceHole
    spliceHoles (X.TextNode t) = mempty
    spliceHoles (X.Comment  c) = mempty
    spliceHoles (X.Element tag attrs chld) =
      let childHoles = concatUiHoles $ spliceHoles <$> chld
      in  case parseTag tag attrs of
        Nothing    -> childHoles
        Just h -> M.insertWith (<>) tag h childHoles


-------------------------------------------------------------------------------
-- Default widget for letting user plug the spliceHoles with values
spliceWidgets
    :: forall t m. MonadWidget t m
    => Dynamic t (M.Map T.Text UiSpliceHole)
    -> m (Dynamic t (H.Splices (HI.Splice IO)))
spliceWidgets spliceHoles = do
    pb <- getPostBuild
    rec let holeHistory    = attach (current spliceHoles) (updated spliceHoles)
            spliceHistory  = attach (current splices) (updated spliceHoles)

            -- Add hole if it wasn't in the old set or its type has changed
            mkAdd x y      = if x /= y
                             then Just (x <> y :: UiSpliceHole)
                             else Nothing
            holesToAdd     = fmap Just . (uncurry (M.differenceWith mkAdd))
                             <$> holeHistory

            -- Drop hole if not in the new set AND its splice is invalid
            -- (don't drop it if its a valid-but-unused splice)
            holesToDrop    = (\(s, h') ->
                                let badHoles  = M.keysSet (M.filter isLeft s)
                                    keepHoles = badHoles \\ M.keysSet h'
                                in  M.fromSet (const Nothing) keepHoles
                             ) <$> spliceHistory

            initialHoles   = fmap Just <$> tag (current spliceHoles) pb
            holeUpdates    = leftmost [ holesToAdd <> holesToDrop
                                      , initialHoles]

            errSpl k v = errorSplice k
        wUpdates  <- listWithKeyShallowDiff mempty holeUpdates
            (\k v dV -> do
                    holeType <- uniqDyn <$> holdDyn v dV
                    ups <- dyn (ffor holeType $ \t ->
                                   dynSpliceWidget (errSpl k t) t)
                    return . join =<< holdDyn (constDyn $ Left $ errSpl k v) ups
            )

        let splices = joinDynThroughMap wUpdates
    return $
        M.foldl' (>>) mempty . M.mapWithKey (M.##) . M.map (either id id) <$>
        splices


-------------------------------------------------------------------------------
-- Widget for building a Splice from a SpliceHole
dynSpliceWidget
  :: forall t m.MonadWidget t m
  => HI.Splice IO
  -> UiSpliceHole
  -- ^ Splice to run on error
  -> m (Dynamic t (Either (HI.Splice IO) (HI.Splice IO)))
dynSpliceWidget errSplice sw = case sw of
  UiSpliceText   -> uiSpliceRead (\t -> if null t
                                        then Nothing
                                        else Just t) (T.pack) "string"
  UiSpliceDouble -> uiSpliceRead (readMaybe :: String -> Maybe Double)
                                 (T.pack . show) "number"
  UiSpliceDropdown xs -> case xs of
    [] -> return $ constDyn $ Left errSplice
    (x:_) -> do
      dd <- value <$> dropdown (fst x) (constDyn $ M.fromList xs) def
      return $ ffor dd $ \d -> Right (return $ [X.TextNode d])
  other -> return $ constDyn $ Left $ errorSplice (T.pack $ show other)
  where
    uiSpliceRead
      :: (String -> Maybe b)
      -> (b -> T.Text)
      -> T.Text
      -> m (Dynamic t (Either (HI.Splice IO) (HI.Splice IO)))
    uiSpliceRead p s inType = do
      tx <- value <$> textInput def { _textInputConfig_inputType = inType }
      return $ ffor tx $ \t ->
        case p (T.unpack t) of
          Nothing -> Left  errSplice
          Just v  -> Right $ return [X.TextNode (s v)]


-------------------------------------------------------------------------------
-- A default parser of DOM nodes into splice holes
spliceHoleParser :: T.Text -> [(T.Text,T.Text)] -> Maybe UiSpliceHole
spliceHoleParser t attrs = case T.stripPrefix "splice:" t of
    Nothing   -> Nothing
    Just name -> case lookup "splicetype" attrs of
        Just "text"     -> Just UiSpliceText
        Just "double"   -> Just UiSpliceDouble
        Just "dropdown" -> Just
          (maybe UiSpliceUntyped (UiSpliceDropdown . parseOpts)
                 (lookup "options" attrs ))
        _ -> Just UiSpliceUntyped
  where parseOpts = fmap (second (T.drop 1) . T.breakOn ",") . T.splitOn ";"


-------------------------------------------------------------------------------
-- Style text as an error message for un-filled splice holes
errorSplice :: T.Text -> HI.Splice IO
errorSplice t =
  return $ [X.Element "span" [("class","error-splice")
                             ,("style","font-weight:bold;")] [X.TextNode t]]
