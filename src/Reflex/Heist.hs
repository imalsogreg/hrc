{-# language OverloadedStrings #-}
{-# language GADTs  #-}
{-# language ScopedTypeVariables #-}

module Reflex.Heist where

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

data HeistDynamicConfig t = HDC
  { _hdcAddTemplate :: Event t (T.Text, H.DocumentFile)
  }

data HeistDynamic t n = HeistDynamic
  { _hdHeistState     :: Dynamic t (H.HeistState n)
  , _hdRenderTemplate :: Dynamic t (M.Map T.Text (T.Text, H.MIMEType))
  , _hdErrors         :: Event t T.Text
  }

heistDynamic :: MonadWidget t m => HeistDynamicConfig t -> m (HeistDynamic t m)
heistDynamic cfg = do

  Right hState0 <- runEitherT . liftIO $ H.initHeist emptyHeistConfig

  -- let templateAdditions = ffor (_hdcAddTemplate cfg) $ \(dp, df) s0 ->
  --       return (HI.addTemplate (T.encodeUtf8 df) (bsGetDoc dp df) Nothing s0)

  -- hState <- foldDyn ($) emptyConfig templateAdditions

  return $ HeistDynamic (constDyn hState0) undefined undefined
