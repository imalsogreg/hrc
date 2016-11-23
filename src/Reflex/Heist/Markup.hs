{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Reflex.Heist.Markup where

-------------------------------------------------------------------------------
import           Control.Lens          (makeLenses, (.~), (^.))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as BS
import           Data.Either (partitionEithers)
import qualified Data.Map              as M
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Heist                 as H
import qualified Heist.Interpreted     as HI
import           Reflex.Dom
import qualified Text.XmlHtml          as X


-------------------------------------------------------------------------------
-- | The markup text of a single Heist template
data MarkupCode = MarkupCode
    { _mcName   :: T.Text
      -- ^ Template name (no '.tpl' extension needed)
    , _mcUrlDir :: T.Text
      -- ^ URL to the directory where this template shoud be saved
    , _mcCode   :: T.Text
      -- ^ Raw text of the template
    } deriving (Eq, Show)
makeLenses ''MarkupCode


-------------------------------------------------------------------------------
data MarkupListConfig t m = MarkupListConfig
    { _markupListConfig_initialTemplates
      :: M.Map Int MarkupCode
    , _markupListConfig_modifyTemplates
      :: Event t (M.Map Int (Maybe MarkupCode))
    , _markupListConfig_drawEntry
      :: Int -> Dynamic t MarkupCode -> m ()
    }


-------------------------------------------------------------------------------
data MarkupList t = MarkupList
    { _markupList_key  :: Dynamic t Int
    , _markupList_Docs :: Dynamic t (M.Map Int MarkupCode)
    }

defDrawEntry :: MonadWidget t m => Int -> Dynamic t MarkupCode -> m ()
defDrawEntry k v = dynText (_mcName <$> v)

-------------------------------------------------------------------------------
markupList
    :: MonadWidget t m
    => MarkupListConfig t m
    -> m (MarkupList t)
markupList (MarkupListConfig t0 dT mkChild) = do
    markups <- foldDyn applyMap t0 dT
    rec sel  <- holdDyn 1 dSel
        dSel <- selectViewListWithKey_ sel markups $ \k v isSel -> do
            item <- fmap fst $ elDynAttr' "div" (markupListAtr <$> isSel) $
                mkChild k v
            return $ domEvent Click item
    return $ MarkupList sel markups


-------------------------------------------------------------------------------
-- TODO: Replace with something nicer. ACE?
markupEditor
    :: MonadWidget t m
    => Dynamic t (Maybe MarkupCode)
    -> m (Event t MarkupCode)
markupEditor code = do
    pb <- getPostBuild
    let name  = uniqDyn (fmap _mcName <$> code)
        dCode = maybe "" _mcCode <$>
            leftmost [tag (current code) pb
                     ,tagPromptlyDyn code (updated name)]
    ta <- textArea $ def & textAreaConfig_setValue .~ dCode
                         & textAreaConfig_attributes .~ constDyn codeareaAttrs
    ups <- debounce 0.1 (updated $ value ta)
    return $ attachWithMaybe (\m m' -> fmap (mcCode .~ m') m)
                             (current code) ups


-------------------------------------------------------------------------------
parseMarkups :: M.Map Int MarkupCode -> ([String], [(T.Text, H.DocumentFile)])
parseMarkups = partitionEithers . M.elems . M.map ingestMarkupCode


-------------------------------------------------------------------------------
-- Load a TemplateCode as a Heist document
ingestMarkupCode
    :: MarkupCode
    -> Either String (T.Text, H.DocumentFile)
ingestMarkupCode mc = (tName,) <$> bsGetDoc fName html
  where
    fName = Just $ mc ^. mcUrlDir
    html  = T.encodeUtf8 $ mc ^. mcCode
    tName = mc ^. mcName


-------------------------------------------------------------------------------
markupListAtr :: Bool -> M.Map T.Text T.Text
markupListAtr sel =
    let brd = bool "" " border-left: 3px solid black;" sel
    in  "style" =: ("display:flex; align-items:center;"
                    <> " background-color: rgba(0,0,0,0.1); padding: 10px;"
                    <> brd)


-------------------------------------------------------------------------------
codeareaAttrs :: M.Map T.Text T.Text
codeareaAttrs = "style" =: "width:400px; height:600px;"


-------------------------------------------------------------------------------
-- | Parse a bytestring into XML or HTML nodes
bsGetDocWith
    :: ParserFun
    -> Maybe T.Text -- ^ Document URL (for rec)
    -> BS.ByteString -- ^ Document contentts
    -> Either String H.DocumentFile
bsGetDocWith parser fUrl bs =
    either (\e -> Left $ errName <> " " <> e)
    (Right . flip H.DocumentFile (T.unpack <$> fUrl))
    (parser errName bs)
  where errName = maybe "[unknown filename]" T.unpack fUrl


-------------------------------------------------------------------------------
-- | Parse a bytestring into HTML
bsGetDoc :: Maybe T.Text -> BS.ByteString -> Either String H.DocumentFile
bsGetDoc = bsGetDocWith X.parseHTML


-------------------------------------------------------------------------------
-- | Parse a bytestring into XML
bsGetXML :: Maybe T.Text -> BS.ByteString -> Either String H.DocumentFile
bsGetXML = bsGetDocWith X.parseXML


-------------------------------------------------------------------------------
type ParserFun = String -> BS.ByteString -> Either String X.Document

