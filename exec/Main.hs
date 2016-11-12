{-# language OverloadedStrings #-}
{-# language GADTs  #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections  #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
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

run :: forall t m.MonadWidget t m => m ()
run = do
  pb <- getPostBuild
  ns <- textInput def
  let myDoc = ("base",) <$> hush (bsGetDoc Nothing testBase)
  hd <- heistDynamic
    (HDC H.emptyHeistConfig ((H.hcNamespace .~) <$> updated (value ns)) (fmapMaybe id (myDoc <$ pb)))
  display (either unlines (const "Ok") <$> (hd ^. hdHeistState))
  return ()

testBase :: BS.ByteString
testBase = "<html><head></head><body><h1>Hello world!</h1><h:test/></body></html>"

testTemplate1 :: BS.ByteString
testTemplate1 = "<h1>Testing</h1>"

testTemplate2 :: BS.ByteString
testTemplate2 = "<h2>Testing again</h2>"

hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a
