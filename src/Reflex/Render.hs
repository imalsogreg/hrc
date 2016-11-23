{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Reflex.Render where

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
import           Reflex.Markup

-------------------------------------------------------------------------------
preview
  :: forall t m. MonadWidget t m
  => Dynamic t T.Text
  -> Dynamic t (Either [T.Text] (H.HeistState IO))
  -> Dynamic t (M.Map Int MarkupCode)
  -> m ()
preview templateName hs markups = do
  pb <- getPostBuild
  tName <- pinButton templateName
  let previewData   = (,,) <$> tName <*> hs <*> markups
      runRender (_, Left t, _) = return $ Left (T.unlines t)
      runRender (tName, Right s, ms) = do
        r <- HI.renderTemplate s (T.encodeUtf8 tName)
        return $ maybe
          (Left $ if M.null (M.filter ((== tName) . _mcName) ms)
                  then "Not found"
                  else "Couldn't parse")
          (Right . T.decodeUtf8 . BSL.toStrict . B.toLazyByteString . fst)
          r
  renderUpdates :: Event t (Either T.Text T.Text) <- performEvent $ fmap (liftIO . runRender)
    (leftmost [tag (current previewData) pb, updated previewData])
  lastOkRender <- holdDyn "No render" (fmapMaybe id $ hush <$> renderUpdates)
  elDynAttr "iframe" (("srcdoc" =:) <$> lastOkRender ) blank

pinButton :: MonadWidget t m => Dynamic t T.Text -> m (Dynamic t T.Text)
pinButton t = do
  pb <- getPostBuild
  rec pinning :: Dynamic t Bool <- toggle False (domEvent Click pinDiv)
      pinningT <- holdDyn "" (leftmost [ tag (current t) pb
                                       , gate (current pinning) (updated t)])
      pinDiv  <- fmap fst $ elDynAttr' "div" (pinAttrs <$> pinning) $
        dynText retT
      let retT = join $ bool t pinningT <$> pinning
  return retT

pinAttrs :: Bool -> M.Map T.Text T.Text
pinAttrs = (("style" =:) . ("background-color:" <>)) . bool "white" "green"

-------------------------------------------------------------------------------
-- Reinitialize heist state as templates change
heistState
  :: MonadWidget t m
  => H.HeistConfig IO
  -- -> Dynamic t (M.Map Int MarkupCode)
  -> Dynamic t [(T.Text, H.DocumentFile)]
  -> Dynamic t (H.Splices (HI.Splice IO))
  -> m (Dynamic t (Either [T.Text] (H.HeistState IO)))
heistState cfg docs splices = do
  pb <- getPostBuild
  let docsAndSplices = (,) <$> docs <*> splices
  let cfgUpdates = leftmost [tagPromptlyDyn docsAndSplices pb
                            , updated docsAndSplices]
      bimap f g  = either (Left . f) (Right . g)
  dHs <- performEvent $ ffor cfgUpdates $ \(ms, spls) ->
    let addDocs s = foldr (\(tName,tDoc) hs ->
                             HI.addTemplate (T.encodeUtf8 tName)
                                            (X.docContent $ H.dfDoc tDoc)
                                            (H.dfFile tDoc) hs
                          ) s ms
    in  liftIO $ fmap addDocs
                 <$> H.initHeist (cfg & H.hcInterpretedSplices %~ (<> spls))

  holdDyn (Left ["Not initialized"]) (bimap (fmap T.pack) id <$> dHs)




hush :: Either e a -> Maybe a
hush (Left _)  = Nothing
hush (Right a) = Just a

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e _        = Left e

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing
