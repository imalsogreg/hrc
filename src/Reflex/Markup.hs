{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Reflex.Markup where

import           Control.Lens       (makeLenses, (.~), (^.))
import           Data.Bool          (bool)
import qualified Data.Map           as M
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Heist              as H
import qualified Heist.Interpreted  as HI
import           Reflex.Dom
import qualified Text.XmlHtml       as X


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
    { _markupListConfig_initialTemplates :: M.Map Int MarkupCode
    , _markupListConfig_modifyTemplates  :: Event t (M.Map Int (Maybe MarkupCode))
    , _markupListConfig_drawEntry        :: Int -> Dynamic t MarkupCode -> m ()
    }


-------------------------------------------------------------------------------
data MarkupList t = MarkupList
    { _markupList_key  :: Dynamic t Int
    , _markupList_Docs :: Dynamic t (M.Map Int MarkupCode)
    }


-------------------------------------------------------------------------------
markupList
    :: MonadWidget t m
    => MarkupListConfig t m
    -> m (MarkupList t)
markupList (MarkupListConfig t0 dT mkChild) = do
    markups <- foldDyn applyMap t0 dT
    rec sel  <- holdDyn 0 dSel
        dSel <- selectViewListWithKey_ sel markups $ \k v isSel -> do
            item <- fmap fst $ elDynAttr' "div" (markupListAtr <$> isSel) $
                mkChild k v
            return $ domEvent Click item
    return $ MarkupList sel markups


-------------------------------------------------------------------------------
-- TODO: Replace with something nicer. ACE?
markupEditor
    :: MonadWidget t m
    -> Dynamic t MarkupCode
    -> m (Event t (Either T.Text MarkupCode))
markupEditor code = do
    pb <- getPostBuild
    templateName <- holdDyn "" $ _mcName <$> leftmost [tag code pb, updated pb]
    let extUpdates = ffilter leftmost [attach code pb, updated pb]
    ta <- textArea $ def & textAreaConfig_setValue .~ (_tcCode <$> extUpdates)
                         & textAreaConfig_attributes .~ constDyn codeareaAttrs
    ups <- debounce 0.5 (updated $ value ta)

-------------------------------------------------------------------------------
markupListAtr :: Bool -> M.Map T.Text T.Text
markupListAtr sel =
    let brd = bool "" " border-left: 3px solid black;" sel
    in  "style" =: ("display:flex; align-items:center; background-color: rgba(0,0,0,0.1); padding: 10px;" <> brd)

codeareaAttrs :: M.Map T.Text T.Text
codeareaAttrs = "style" =: "width:400px; height:600px;"
