{-# language OverloadedStrings #-}
{-# language GADTs  #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections  #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Builder as B
import Data.Map.Syntax
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Heist as H
import qualified Heist.Interpreted as HI
import Reflex.Dom
import Reflex.Heist

main :: IO ()
main = mainWidget run


  -- do
  --   x <- liftIO $ H.initHeist (H.emptyHeistConfig & H.hcErrorNotBound .~ False
  --                                                 & H.hcNamespace     .~ "")
  --   -- print x
  --   case x of
  --     Right hc -> HI.renderTemplate hc "base" >> print "Done"
  --     Left es  -> print es


run :: forall t m .MonadWidget t m => m ()
run = do
  pb <- getPostBuild
  ns <- textInput def
  let myDoc = ("base" :: T.Text,) <$> hush (bsGetDoc Nothing testBase)
  hd  <- heistDynamic
                          (HDC H.emptyHeistConfig
                           ((H.hcNamespace .~) <$> updated (value ns))
                           (traceEvent "hi" $ fmapMaybe id (myDoc <$ pb))
                          )
  divClass "" $ display (either unlines (const "Ok") <$> (hd ^. hdHeistState))
  divClass "" $ display (fmap H.templateNames <$> (hd ^. hdHeistState))
  performEvent_ $ ffor (updated $ hd ^. hdHeistState) $
    either (liftIO . putStrLn . unlines) (\s -> do
                         t <- liftIO $ previewTemplate "base" s
                         liftIO $ print t
                     )
  return ()

previewTemplate :: BS.ByteString -> H.HeistState IO -> IO String
previewTemplate t s = do
  r <- HI.renderTemplate (HI.bindSplices someSplices s) t
  case r of
    Nothing    -> return "Template not found"
    Just (b,_) -> return (T.unpack . T.decodeUtf8 . BSL.toStrict . B.toLazyByteString $ b)

testBase :: BS.ByteString
testBase = "<html><head></head><body><h1>Hello world!</h1><bind tag=\"test\">bind</bind><test /></body></html>"

testTemplate1 :: BS.ByteString
testTemplate1 = "<h1>Testing</h1>"

testTemplate2 :: BS.ByteString
testTemplate2 = "<h2>Testing again</h2>"

hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a

someSplices :: H.Splices (HI.Splice IO)
someSplices = do
  "test" ## HI.textSplice "SUCCESS"
