{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import           Control.Arrow                          (first)
import           Control.Lens
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Class              (lift)
import qualified Data.Binary.Builder                    as B
import           Data.Bool                              (bool)
import qualified Data.ByteString.Char8                  as BS
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.Map                               as M
import           Data.Map.Syntax                        (( ## ))
import qualified Data.Map.Syntax                        as M
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            ((<>))
import           Data.String.QQ
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import           Data.Tuple                             (swap)
import qualified Heist                                  as H
import qualified Heist.Interpreted                      as HI
import           Reflex.Dom
import qualified Reflex.Dom.Contrib.Widgets.EditInPlace as EIP
import           Reflex.Heist

-------------------------------------------------------------------------------
main :: IO ()
main = mainWidget run


-------------------------------------------------------------------------------
run :: forall t m .MonadWidget t m => m ()
run = mdo
    text "3"
    pb <- getPostBuild
    -- ns <- textInput def
    -- let myDoc = ("base" :: T.Text,) <$> hush (bsGetDoc Nothing testBase)
    hd  <- heistDynamic
           (HDC (H.emptyHeistConfig
                 & H.hcInterpretedSplices .~ (someSplices <> H.defaultInterpretedSplices)
                 & H.hcNamespace .~ "")
               never
               -- ((H.hcNamespace .~) <$> updated (value ns))
               templates0
               ups
               "takt"
               "splicetype"
               mempty
               never
           )
    -- divClass "" $ display (fmap H.templateNames <$> (hd ^. hdHeistState))
    -- performEvent_ $ ffor (updated $ hd ^. hdHeistState) $
    --     either (liftIO . putStrLn . unlines)
    --     (\s -> do
    --             t <- liftIO $ previewTemplate "base" s
    --             liftIO $ print t
    --     )
    ups <- divClass "" $ templatesView hd
    return ()


-------------------------------------------------------------------------------
templatesView
    :: forall t m. MonadWidget t m
    => HeistDynamic t
    -> m (Event t (M.Map Int (Maybe TemplateCode)))
templatesView s = mdo
    dk <- holdDyn (Just 2) (Just <$> fst tl)
    let ts = _hdTemplates s
    tl <- templateList dk s
    (tTex,tPrev) <- divClass "" $ do
        a <- templateCode dk s
        b <- templatePreview dk s
        return (a,b)
    let tUpdates = ffor tTex $ \(i,tc) -> i =: Just tc
    return $ leftmost [tUpdates, snd tl]


-------------------------------------------------------------------------------
templateList
    :: MonadWidget t m
    => Dynamic t (Maybe Int)
    -> HeistDynamic t
    -> m (Event t Int, Event t (M.Map Int (Maybe TemplateCode)))
templateList dk hd = do

    let ts        = hd ^. hdTemplates
        hs        = hd ^. hdHeistState
    l <- selectViewListWithKey (fromMaybe (-1) <$> dk) ts $ \k v isSel -> do

        (listing,nn) <- elDynAttr' "div" (listingItemAttrs <$> isSel) $ do

            let pOk = parseOk <$> (hd ^. hdHeistState) <*> v
                parseInd = indAttrs <$> pOk

            elDynAttr "div" parseInd blank
            EIP.editInPlace (current isSel) (_tcName <$> v)

        let nameUpdate = attachWith (\tc n -> Just $ tc {_tcName = n}) (current v) nn
            selUpdate  = Nothing <$ domEvent Click listing

        return $ leftmost [selUpdate, nameUpdate] -- (selUpdate, nameUpdate)

    let sels = fforMaybe l $ \case
            (k,Nothing) -> Just k
            _           -> Nothing
        nns  = fforMaybe l $ \case
            (k, Just n) -> Just $ k =: Just n
            _           -> Nothing
    return (sels,nns)


-- TODO: Rewrite templateCode w/ this signature?
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
        textUpdates = leftmost [updated codeText, tag (current codeText) pb]
    ta <- textArea $ def & textAreaConfig_setValue .~ textUpdates
                         & textAreaConfig_attributes .~ constDyn codeareaAttrs
    tbUpdates <- debounce 0.5 . updated $ value ta -- Debounce is critical
    -- upButton <- button "Update"
    -- let tbUpdates = tag (current $ value ta) upButton
    let goodUpdates :: Event t (T.Text,TemplateCode) =
            fmapMaybe (sequence . swap) $
            attach (current thisTemplate) tbUpdates
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
templatePreview dk (HeistDynamic hs _ hts sps) = do
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
    elDynAttr "iframe" ((iframeAttrs <>) . ("srcdoc" =:) <$> innerHtml) blank

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
    <test />
    <test2 />
  </body>
</html>
|]


testTemplate1 :: BS.ByteString
testTemplate1 = [s|
<h1>Testing</h1>

<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum hendrerit
sem ac tincidunt scelerisque. Donec tempus est tincidunt, posuere nibh vitae,
placerat turpis. Nulla orci ante, euismod ut ligula vitae, interdum consectetur
urna. Maecenas non finibus risus. Mauris porttitor interdum aliquam. Donec
posuere pulvinar nisi, vitae laoreet mi pulvinar porttitor. Sed maximus
suscipit blandit. Mauris sagittis sapien sit amet est vulputate vestibulum.
Donec id risus vitae dolor tristique imperdiet nec nec urna. Etiam pellentesque,
quam non imperdiet egestas, sem lacus tincidunt purus, a pulvinar eros odio ut
ante. Mauris pulvinar felis at risus pretium, vitae molestie metus iaculis.
Phasellus tempor mollis ante. Proin magna orci, varius eu malesuada quis, congue
vitae sapien. Ut in eleifend quam, quis mattis risus.
e
</p>

<img src="https://pbs.twimg.com/profile_images/1356905591/ichiro_itano1_1000_bigger.jpeg"/>
|]

testTemplate2 :: BS.ByteString
testTemplate2 = "<h2>Testing again</h2>"

someSplices :: H.Splices (HI.Splice IO)
someSplices = do
  "test" ## HI.textSplice "SUCCESS"

tShow :: Show a => a -> T.Text
tShow = T.pack . show

codeareaAttrs :: M.Map T.Text T.Text
codeareaAttrs = "style" =: "width:400px; height:600px;"

iframeAttrs :: M.Map T.Text T.Text
iframeAttrs   = "width" =: "400px" <> "height" =: "600px;"

indAttrs :: Bool -> M.Map T.Text T.Text
indAttrs b = let bgc = bool "rgba(255,0,0,0.5)" "rgba(0,255,0,0.5);" b
                 shc = bool "rgba(255,0,0,1)" "rgba(0,255,0,1);" b
             in  "style" =: ("margin-right: 10px; width: 8px; height: 8px; border-radius:8px; border: 1px solid white; background-color: " <> bgc <> "; box-shadow: " <> shc <> ";")

parseOk :: Either [String] (H.HeistState IO) -> TemplateCode -> Bool
parseOk (Left _) _   = False
parseOk (Right hs) t = T.encodeUtf8 (_tcName t) `elem` (head
    <$> H.templateNames hs)

listingItemAttrs :: Bool -> M.Map T.Text T.Text
listingItemAttrs sel =
    let brd = bool "" " border-right: 3px solid black;" sel
    in  "style" =: ("display:flex; align-items:center; background-color: rgba(0,0,0,0.1); padding: 10px;" <> brd)
