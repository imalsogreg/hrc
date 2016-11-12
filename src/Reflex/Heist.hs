{-# language OverloadedStrings #-}
{-# language GADTs  #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language RankNTypes #-}

module Reflex.Heist where

import Control.Applicative (liftA2)
import Data.Foldable (foldl')
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Either (either)
import qualified Data.Map as M
import           Data.Maybe (maybe)
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Heist as H
import qualified Heist.Interpreted as HI
import qualified Text.XmlHtml as X
import           Reflex.Dom


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

bsGetDoc :: Maybe T.Text -> BS.ByteString -> Either String H.DocumentFile
bsGetDoc = bsGetDocWith X.parseHTML

bsGetXML :: Maybe T.Text -> BS.ByteString -> Either String H.DocumentFile
bsGetXML = bsGetDocWith X.parseXML

type ParserFun = String -> BS.ByteString -> Either String X.Document

data HeistDynamicConfig t n = HDC
  { _hdcInitialConfig :: H.HeistConfig n
  , _hdcModifyConfig  :: Event t (H.HeistConfig n -> H.HeistConfig n)
  , _hdcAddTemplate :: Event t (T.Text, H.DocumentFile)
  }

makeLenses ''HeistDynamicConfig

data HeistDynamic t n = HeistDynamic
  { _hdHeistState     :: Dynamic t (Either [String] (H.HeistState n))
  , _hdRenderTemplate :: Dynamic t (M.Map T.Text (T.Text, H.MIMEType))
  , _hdErrors         :: Event t T.Text
  }

makeLenses ''HeistDynamic

heistDynamic :: forall t m.MonadWidget t m => HeistDynamicConfig t m -> m (HeistDynamic t m)
heistDynamic cfg = do

  let conf0      = cfg ^. hdcInitialConfig
      addT (n,d) = HI.addTemplate
                   (T.encodeUtf8 n) ((X.docContent . H.dfDoc) d) Nothing

  hConfig <- foldDyn ($) conf0 (cfg ^. hdcModifyConfig)
  hDocs   <- foldDyn (:) mempty (cfg ^. hdcAddTemplate)
  hState0 <- liftIO $ H.initHeist conf0

  -- Reinitialize the heist state every time HeistConfig changes or a document is added
  -- This is inefficient
  dState  <- performEvent $ ffor (updated $ liftA2 (,) hConfig hDocs) $ \(conf, docs) ->
    fmap (\goodState -> foldr (\(n,d) s -> addT (n,d) s) goodState docs) <$> liftIO (H.initHeist conf)
  hState <- holdDyn hState0 dState

  return $ HeistDynamic hState undefined undefined
