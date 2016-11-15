{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import           Control.Arrow             (first)
import           Control.Lens
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Binary.Builder       as B
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Map                  as M
import           Data.Map.Syntax           (( ## ))
import qualified Data.Map.Syntax           as M
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.String.QQ
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Tuple                (swap)
import qualified Heist                     as H
import qualified Heist.Interpreted         as HI
import           Reflex.Dom
import           Reflex.Heist

-------------------------------------------------------------------------------
main :: IO ()
main = mainWidget run


-------------------------------------------------------------------------------
run :: forall t m .MonadWidget t m => m ()
run = mdo
    pb <- getPostBuild
    ns <- textInput def
    let myDoc = ("base" :: T.Text,) <$> hush (bsGetDoc Nothing testBase)
    hd  <- heistDynamic
           (HDC (H.emptyHeistConfig
                 & H.hcInterpretedSplices .~ someSplices
                 & H.hcNamespace .~ "")
               ((H.hcNamespace .~) <$> updated (value ns))
               templates0
               ups
           )
    -- divClass "" $ display (either unlines (const "Ok") <$> (hd ^. hdHeistState))
    divClass "" $ display (fmap H.templateNames <$> (hd ^. hdHeistState))
    performEvent_ $ ffor (updated $ hd ^. hdHeistState) $
        either (liftIO . putStrLn . unlines)
        (\s -> do
                t <- liftIO $ previewTemplate "base" s
                liftIO $ print t
        )
    ups <- divClass "" $ templatesView hd
    return ()


-------------------------------------------------------------------------------
templatesView
    :: forall t m. MonadWidget t m
    => HeistDynamic t
    -> m (Event t (M.Map Int (Maybe TemplateCode)))
templatesView s = do
    let dk = constDyn (Just 2)
    let ts = _hdTemplates s
    tl <- templateList dk s
    (tTex,tPrev) <- divClass "" $ do
        a <- templateCode dk s
        b <- templatePreview dk s
        return (a,b)
    let tUpdates = ffor tTex $ \(i,tc) -> i =: Just tc
    return (tUpdates)


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


-- templateCode
--     :: forall t m. MonadWidget t m
--     -> Int
--     -> TemplateCode
--     -> m (Event t (Int, TemplateCode))
-- templateCode t tc = do

-------------------------------------------------------------------------------
templateCode
    :: forall t m. MonadWidget t m
    => Dynamic t (Maybe Int)
    -> HeistDynamic t
    -> m (Event t (Int, TemplateCode))
templateCode dk s = do
    pb <- getPostBuild
    let dts = s ^. hdTemplates
        thisTemplate :: Dynamic t (Maybe TemplateCode)
        thisTemplate = (\k ts -> k >>= flip M.lookup ts) <$> dk <*> dts
    el "div" $ text "Template Code"
    let codeText :: Dynamic t T.Text =
            zipDynWith (\k ts -> maybe "" _tcCode (k >>= flip M.lookup ts)) dk dts
        textUpdates = leftmost [updated codeText, tagPromptlyDyn codeText pb]
    ta <- textArea $ def & textAreaConfig_setValue .~ textUpdates
    -- tbUpdates <- debounce 0.5 . updated $ value ta
    upButton <- button "Update"
    let tbUpdates = tag (current $ value ta) upButton
    let goodUpdates :: Event t (T.Text,TemplateCode) =
            fmapMaybe (sequence . swap) $
            attachPromptlyDyn thisTemplate tbUpdates
        up' :: Event t (Int,TemplateCode)
        up' = fforMaybe (attachPromptlyDyn dk goodUpdates) $ \(mk, (c',c)) -> case mk of
            Nothing -> Nothing
            Just k  -> Just (k, c { _tcCode = c'})
    return up'

templatePreview
    :: MonadWidget t m
    => Dynamic t (Maybe Int)
    -> HeistDynamic t
    -> m ()
templatePreview dk (HeistDynamic hs _ hts) = do
    pb <- getPostBuild
    let d   = (,,) <$> dk <*> hts <*> hs
        runPreview :: (Maybe Int, M.Map Int TemplateCode, Either [String] (H.HeistState IO)) -> IO T.Text
        runPreview (mi, ts, es) = either return (uncurry previewTemplate) $ do
            s <- either (Left . T.pack . unlines) Right es
            i <- note "No template selected" mi
            tc <- note ("No such TemplateCode" <> tShow i) (M.lookup i ts)
            return (_tcName tc, s)
    dInnerHtml <- performEvent $ ffor (leftmost [updated d, tagPromptlyDyn d pb]) $ \(k,ts,s) ->
        -- maybe (return "No entry") (liftIO . uncurry previewTemplate . first _tcName)
        -- ((,,) <$> (k >>= flip M.lookup ts) <*> hush s)
        liftIO (runPreview (k,ts,s))
    innerHtml <- holdDyn "Not rendered" dInnerHtml
    elDynAttr "iframe" (("srcdoc" =:) <$> innerHtml) blank

templates0 :: M.Map Int TemplateCode
templates0 =
       1 =: TemplateCode "base" "src"  (T.decodeUtf8 testBase)
    <> 2 =: TemplateCode "test1" "src" (T.decodeUtf8 testTemplate1)
    <> 3 =: TemplateCode "test2" "src" (T.decodeUtf8 testTemplate2)

testBase :: BS.ByteString
testBase = [s|
<html>
  <head></head>
  <body>
    Hello.
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

tShow :: Show a => a -> T.Text
tShow = T.pack . show
