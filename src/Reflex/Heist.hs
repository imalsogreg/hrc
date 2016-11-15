{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Reflex.Heist where

import           Control.Applicative    (liftA2)
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Binary.Builder    as B
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Either            (either)
import           Data.Foldable          (foldl')
import qualified Data.Map               as M
import qualified Data.Map.Syntax        as M
import           Data.Maybe             (catMaybes, maybe)
import           Data.Monoid
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Heist                  as H
import qualified Heist.Interpreted      as HI
import           Reflex.Dom
import qualified Text.XmlHtml           as X


-------------------------------------------------------------------------------
-- | The interfaces to a single Heist template
data TemplateCode = TemplateCode
    { _tcName   :: T.Text
      -- ^ Template name (no '.tpl' extension needed)
    , _tcUrlDir :: T.Text
      -- ^ URL to the directory where this template shoud be saved
    , _tcCode   :: T.Text
      -- ^ Raw text of the template
    } deriving (Eq, Show)
makeLenses ''TemplateCode


-------------------------------------------------------------------------------
-- | Heist widget inputs
data HeistDynamicConfig t = HDC
    { _hdcInitialConfig   :: H.HeistConfig IO
      -- ^ Initial HeistConfig
    , _hdcModifyConfig    :: Event t (H.HeistConfig IO -> H.HeistConfig IO)
      -- ^ Other modifications to the HeistConfig
    , _hdcInitialTemplates :: M.Map Int TemplateCode
      -- ^ Baseline Heist configuration
    , _hdcModifyTemplates :: Event t (M.Map Int (Maybe TemplateCode))
      -- ^ Addition and deletion of TemplateCode items
    }
makeLenses ''HeistDynamicConfig


-------------------------------------------------------------------------------
-- | Heist widget state
data HeistDynamic t = HeistDynamic
    { _hdHeistState :: Dynamic t (Either [String] (H.HeistState IO))
      -- ^ HeistState, indicating successful template loading and allowing
      --   templates to be rendered
    , _hdConfig     :: Dynamic t (H.HeistConfig IO)
      -- ^ HeistConfig, for examining loaded template info
    , _hdTemplates  :: Dynamic t (M.Map Int TemplateCode)
      -- ^ TemplateCode entries
    }

makeLenses ''HeistDynamic


-------------------------------------------------------------------------------
-- | Heist widget
heistDynamic
    :: forall t m.MonadWidget t m
    => HeistDynamicConfig t
    -> m (HeistDynamic t)
heistDynamic (HDC cfg0 dCfg tpls0 dTmpls) = do

    hDocs <- foldDyn applyMap tpls0 dTmpls

    hConfig <- foldDyn ($) cfg0 dCfg

    hState0 <- liftIO $ processConfig cfg0 tpls0
    dState <- performEvent $ ffor (updated $ (,) <$> hConfig <*> hDocs) $
        liftIO . uncurry processConfig
    hState <- holdDyn hState0 dState

    return $ HeistDynamic hState hConfig hDocs

processConfig :: H.HeistConfig IO -> M.Map Int TemplateCode -> IO (Either [String] (H.HeistState IO))
processConfig cfg ds = do
    s <- H.initHeist cfg
    return $ flip addDocs ts <$> s
      where
        ts = catMaybes . M.elems $ M.map (hush . ingestTemplateCode) ds
        addDoc :: (T.Text, H.DocumentFile) -> H.HeistState IO -> H.HeistState IO
        addDoc (n,doc) = HI.addTemplate
                         (T.encodeUtf8 n)
                         ((X.docContent . H.dfDoc) doc)
                         (H.dfFile doc)

        addDocs :: H.HeistState IO -> [(T.Text, H.DocumentFile)] -> H.HeistState IO
        addDocs s ds = foldl' (flip addDoc) s ds


hush :: Either e a -> Maybe a
hush (Left _)  = Nothing
hush (Right a) = Just a

-------------------------------------------------------------------------------
-- Render a template to a string
previewTemplate :: BS.ByteString -> H.HeistState IO -> IO String
previewTemplate t s = do
  r <- HI.renderTemplate s t
  case r of
    Nothing    -> return "Template not found"
    Just (b,_) -> return (T.unpack . T.decodeUtf8 . BSL.toStrict . B.toLazyByteString $ b)


-------------------------------------------------------------------------------
-- Load a TemplateCode as a Heist document
ingestTemplateCode
    :: TemplateCode
    -> Either String (T.Text, H.DocumentFile)
ingestTemplateCode tc = (tName,) <$> bsGetDoc fName html
  where
    fName = Just $ tc ^. tcUrlDir
    html  = T.encodeUtf8 $ tc ^. tcCode
    tName = tc ^. tcName


-------------------------------------------------------------------------------
-- Utilities
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

-- | Parse a bytestring into HTML
bsGetDoc :: Maybe T.Text -> BS.ByteString -> Either String H.DocumentFile
bsGetDoc = bsGetDocWith X.parseHTML

-- | Parse a bytestring into XML
bsGetXML :: Maybe T.Text -> BS.ByteString -> Either String H.DocumentFile
bsGetXML = bsGetDocWith X.parseXML

type ParserFun = String -> BS.ByteString -> Either String X.Document
