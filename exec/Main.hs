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
import           Control.Monad                          (join)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Class              (lift)
import qualified Data.Binary.Builder                    as B
import           Data.Bool                              (bool)
import qualified Data.ByteString.Char8                  as BS
import qualified Data.ByteString.Lazy                   as BSL
import           Data.Either                            (isRight)
import qualified Data.Map                               as M
import           Data.Map.Syntax                        (( ## ))
import qualified Data.Map.Syntax                        as M
import           Data.Maybe                             (fromMaybe, isJust)
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
import           Text.Read                              (readMaybe)
import qualified Text.XmlHtml                           as X

-------------------------------------------------------------------------------
main :: IO ()
main = mainWidget run


-------------------------------------------------------------------------------
run :: forall t m .MonadWidget t m => m ()
run = mdo
    text "6"
    pb <- getPostBuild
    hd  <- heistDynamic
           (HDC (H.emptyHeistConfig
                 & H.hcInterpretedSplices .~ (someSplices <> H.defaultInterpretedSplices)
                 & H.hcNamespace .~ "")
               never
               templates0
               ups
               spls
               "takt"
               "splicetype"
               mempty
               never
           )
    (ups, spls) <- divClass "" $ templatesView hd
    return ()


-------------------------------------------------------------------------------
templatesView
    :: forall t m. MonadWidget t m
    => HeistDynamic t
    -> m (Event t (M.Map Int (Maybe TemplateCode)), Dynamic t (H.Splices (HI.Splice IO)))
templatesView s = mdo
    dk <- holdDyn (Just 2) (Just <$> fst tl)
    let ts = _hdTemplates s
    (tl,sl) <- divClass "template-listing" $ do
        tl <- templateList dk s
        sl <- spliceList s
        return (tl,sl)
    (tTex,tPrev) <- divClass "" $ do
        a <- templateCode dk s
        b <- templatePreview dk s
        return (a,b)
    let tUpdates = ffor tTex $ \(i,tc) -> i =: Just tc
    return (leftmost [tUpdates, snd tl], sl)


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


-------------------------------------------------------------------------------
spliceList
  :: MonadWidget t m
  => HeistDynamic t
  -> m (Dynamic t (H.Splices (HI.Splice IO)))
spliceList s = do
    pb <- getPostBuild
    let ws  = s ^. hdDynamicSplices
        dWs = leftmost [ tag (current $ M.map Just <$> ws) pb
                       , updated (M.map Just <$> ws)]
    dSpls <- holdDyn mempty dWs
    let diffs = attachWith (diffMap) (current dSpls) dWs
    spls <- listWithKeyShallowDiff mempty diffs viewDynamicSplice
    return $ M.foldl' (>>) mempty . M.mapWithKey (M.##) . M.map (either id id) <$>
        joinDynThroughMap spls


-------------------------------------------------------------------------------
viewDynamicSplice
  :: forall t m.MonadWidget t m
  => T.Text
  -> Maybe UiSpliceWidget
  -> Event t (Maybe UiSpliceWidget)
  -> m (Dynamic t (Either (HI.Splice IO) (HI.Splice IO)))
viewDynamicSplice k wType dWType = do
    w <- widgetHold (uiSpliceWidget wType) (uiSpliceWidget <$> dWType)
    return $ join w
    where
    uiSpliceWidget thisW = elAttr "div" ("class" =: "single-splice" <> listingItemAttrs False) $ mdo
              elDynAttr "div" (indAttrs . isRight <$> dspl) blank
              text k
              dspl <- case wType of
                          Just UiSpliceText -> do
                              tx <- value <$> textInput def
                              return $ ffor tx $ \t ->
                                  if   T.null t
                                  then Left $ errorSplice $ "<" <> k <> " :: " <> "text>"
                                  else Right $ return [X.TextNode t]
                          Just UiSpliceDouble -> do
                              tx <- value <$> textInput def
                                    { _textInputConfig_inputType = "number" }
                              return $ ffor tx $ \t ->
                                  if isJust . (readMaybe :: String -> Maybe Int) . T.unpack $ t
                                  then Right . return . (:[]) $ X.TextNode t
                                  else Left $ errorSplice $ "<" <> k <> " :: " <> "double>"
                          Just (UiSpliceDropdown ds) -> do
                              tx <- value <$> dropdown "Tall Latte"
                                  (constDyn $ M.fromList ds) def
                              return $ ffor tx $ \t ->
                                  if isJust $ lookup t ds
                                  then Right . return . (:[]) $ X.TextNode t
                                  else Left $ errorSplice $ "<" <> k <> " :: " <> "drink-dropdown>"
              return dspl

errorSplice :: T.Text -> HI.Splice IO
errorSplice t = return $ [X.Element "span" [("class","error-splice")
                                           ,("style","font-weight:bold;")] [X.TextNode t]]

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
templatePreview dk (HeistDynamic hs _ hts spls) = do
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
       1 =: TemplateCode "block" "src"  (T.decodeUtf8 testBase)
    <> 2 =: TemplateCode "test1" "src" (T.decodeUtf8 testTemplate1)
    <> 3 =: TemplateCode "fancydrink" "src" (T.decodeUtf8 testTemplate2)

testBase :: BS.ByteString
testBase = [s|
<div class="block">
  <apply-content/>
</div>

<style>
.block {
  border-left: 1px solid black;
  background-color: rgba(0,0,0,0.1);
  margin: 5px;
}
</style>
|]


testTemplate1 :: BS.ByteString
testTemplate1 = [s|
<h1>A test template</h1>

Hi <taktname splicetype="text"/>

<apply template="block">
Your next <apply template="fancydrink"/>
earns <taktNumStars splicetype="double"/> Stars!
</apply>

<apply  template="block">
<p>
 With
 <img style="height:20px;"
   src="https://img0.etsystatic.com/000/2/5454342/il_214x170.175163732.jpg"/>
 from Starfox
</p>
</apply>

<p>
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum hendrerit
sem ac tincidunt scelerisque. Donec tempus est tincidunt, posuere nibh vitae,
placerat turpis. Nulla orci ante, euismod ut ligula vitae, interdum consectetur
urna. Maecenas non finibus risus. Mauris porttitor interdum aliquam. Donec
posuere pulvinar nisi, vitae laoreet mi pulvinar porttitor. Sed maximus
suscipit blandit. Mauris sagittis sapien sit amet est vulputate vestibulum.
Donec id risus vitae dolor tristique imperdiet nec nec urna.</p>

<img src="https://pbs.twimg.com/profile_images/1356905591/ichiro_itano1_1000_bigger.jpeg"/>
|]

testTemplate2 :: BS.ByteString
testTemplate2 = [s|
<span class="drink">
  <em>
    <taktdrink splicetype="drink-dropdown"/>
  </em>
</span>

<style>
.drink {
  text-shadow: 0px 0px 5px rgba(0,255,250,1);
  color: black;
}
</style>
|]

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
    let brd = bool "" " border-left: 3px solid black;" sel
    in  "style" =: ("display:flex; align-items:center; background-color: rgba(0,0,0,0.1); padding: 10px;" <> brd)
