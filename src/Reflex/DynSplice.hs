{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Reflex.DynSplice where

-------------------------------------------------------------------------------
import           Control.Applicative    (Alternative, liftA2, (<|>))
import           Control.Lens
import           Control.Monad          (join)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Binary.Builder    as B
import           Data.Bool              (bool)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Either            (either)
import           Data.Foldable          (foldl')
import qualified Data.Map               as M
import qualified Data.Map.Syntax        as M
import           Data.Maybe             (catMaybes, fromMaybe, maybe)
import           Data.Monoid
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

instance Monoid UiSpliceHole where
  mempty = UiSpliceUntyped
  UiSpliceUntyped `mappend` x = x
  x               `mappend` _ = x


-------------------------------------------------------------------------------
-- Utility function for combining the results of multiple splice-hole sets
concatUiHoles :: [M.Map T.Text UiSpliceHole] -> M.Map T.Text UiSpliceHole
concatUiHoles = M.unionsWith (<>)


-------------------------------------------------------------------------------
-- Traverse all templates looking for splice requests
collectHoles
  :: (T.Text -> [(T.Text,T.Text)] -> Maybe (T.Text, UiSpliceHole))
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

    spliceHoles :: X.Node -> M.Map T.Text UiSpliceHole
    spliceHoles (X.TextNode t) = mempty
    spliceHoles (X.Comment  c) = mempty
    spliceHoles (X.Element tag attrs chld) =
      let childHoles = concatUiHoles $ spliceHoles <$> chld
      in  case parseTag tag attrs of
        Nothing    -> childHoles
        Just (n,h) -> M.insertWith (<>) n h childHoles


-------------------------------------------------------------------------------
-- Widget for building a Splice from a SpliceHole
dynSpliceWidget
  :: forall t m.MonadWidget t m
  => UiSpliceHole
  -> HI.Splice IO
  -- ^ Splice to run on error
  -> m (Dynamic t (Either (HI.Splice IO) (HI.Splice IO)))
dynSpliceWidget sw errSplice = case sw of
  UiSpliceText   -> uiSpliceRead Just (T.pack)
  UiSpliceDouble -> uiSpliceRead (readMaybe :: String -> Maybe Double)
                                 (T.pack . show)
  UiSpliceDropdown xs -> case xs of
    [] -> return $ constDyn $ Left errSplice
    (x:_) -> do
      dd <- value <$> dropdown (fst x) (constDyn $ M.fromList xs) def
      return $ ffor dd $ \d -> Right (return $ [X.TextNode d])
  where
    uiSpliceRead
      :: (String -> Maybe b)
      -> (b -> T.Text)
      -> m (Dynamic t (Either (HI.Splice IO) (HI.Splice IO)))
    uiSpliceRead p s = do
      tx <- value <$> textInput def
      return $ ffor tx $ \t ->
        case p (T.unpack t) of
          Nothing -> Left  errSplice
          Just v  -> Right $ return [X.TextNode (s v)]