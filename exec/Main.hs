{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Binary.Builder       as B
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Map                  as M
import qualified Data.Map.Syntax           as M
import           Data.Maybe                (fromMaybe)
import           Data.String.QQ
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Heist                     as H
import qualified Heist.Interpreted         as HI
import           Reflex.Dom
import           Reflex.Heist

-------------------------------------------------------------------------------
main :: IO ()
main = mainWidget run


-------------------------------------------------------------------------------
run :: forall t m .MonadWidget t m => m ()
run = do
    pb <- getPostBuild
    ns <- textInput def
    let myDoc = ("base" :: T.Text,) <$> hush (bsGetDoc Nothing testBase)
    hd  <- heistDynamic
           (HDC (H.emptyHeistConfig
                 & H.hcInterpretedSplices .~ someSplices
                 & H.hcNamespace .~ "")
               ((H.hcNamespace .~) <$> updated (value ns))
               templates0
               never
           )
    -- divClass "" $ display (either unlines (const "Ok") <$> (hd ^. hdHeistState))
    divClass "" $ display (fmap H.templateNames <$> (hd ^. hdHeistState))
    performEvent_ $ ffor (updated $ hd ^. hdHeistState) $
        either (liftIO . putStrLn . unlines)
        (\s -> do
                t <- liftIO $ previewTemplate "base" s
                liftIO $ print t
        )
    return ()


-------------------------------------------------------------------------------
templatesView
    :: forall t m. MonadWidget t m
    => HeistDynamic t
    -> m ()
templatesView s = do
    let dk = constDyn (Just 0)
    let ts = _hdTemplates s
    tl <- templateList dk s
    (tTex,tPrev) <- divClass "" $ do
        a <- templateCode dk s
        b <- templatePreview dk s
        return (a,b)
    return ()


-------------------------------------------------------------------------------
templateList
    :: MonadWidget t m
    => Dynamic t (Maybe Int)
    -> HeistDynamic t
    -> m ()
templateList dk hd = do
    selectViewListWithKey (fromMaybe (-1) <$> dk) (hd ^. hdTemplates) $ \k v isSel -> do
        divClass "" (text . T.pack . show $ k)
        return never
    return ()


-------------------------------------------------------------------------------
templateCode
    :: MonadWidget t m
    => Dynamic t (Maybe Int)
    -> HeistDynamic t
    -> m ()
templateCode dk s = do
    pb <- getPostBuild
    let dts = s ^. hdTemplates
    el "div" $ text "Template Code"
    let codeText    = zipDynWith (\k ts -> fromMaybe "" (k >>= flip M.lookup ts)) dk dts
        textUpdates = leftmost [updated codeText, tagDyn codeText pb]
    ta <- textArea $ def & textAreaConfig_setValue .~ (_tcCode <$> textUpdates)
    tbUpdates <- debounce 0.5 . updated $ value ta
    return ()

templatePreview
    :: MonadWidget t m
    => Dynamic t (Maybe Int)
    -> HeistDynamic t
    -> m ()
templatePreview dk ds = do
    let dhs = ds ^. hdTemplates
    innerhtml <- performEvent $ ffor (updated $ (,) dk dhs)
        (liftIO . uncurry previewTemplate)
    elDynAttr "iframe" (("srcdoc" =:) <$> innerhtml) blank

templates0 =
       1 =: TemplateCode "base" "src" templateBase
    <> 2 =: TemplateCode "test1" "src" testTemplate1
    <> 3 =: TemplateCode "test2" "src" testTemplate2

testBase :: BS.ByteString
testBase = [s|
<html>
  <head></head>
  <body>
    <h1>Hello world!</h1>
    <bind tag=\"test2\">bound</bind>
    <test />
    <test2 />
  </body>
</html>
|]


testTemplate1 :: BS.ByteString
testTemplate1 = "<h1>Testing</h1>"

testTemplate2 :: BS.ByteString
testTemplate2 = "<h2>Testing again</h2>"

someSplices :: H.Splices (HI.Splice IO)
someSplices = do
  "test" ## HI.textSplice "SUCCESS"
