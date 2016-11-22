{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Reflex.FullWidget where

import           Control.Lens          (makeLenses, (.~), (^.))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Heist                 as H
import qualified Heist.Interpreted     as HI
import           Reflex.Dom
import qualified Text.XmlHtml          as X

import Reflex.Markup
import Reflex.DynSplice
import Reflex.Render

data HeistWidgetConfig t = HeistWidgetConfig
    { _heistWidgetConfig_initialMarkup :: M.Map Int MarkupCode
    , _heistWidgetConfig_modifyMarkup :: Event t (M.Map Int (Maybe MarkupCode))
    }

data HeistWidget = HeistWidget

heistWidget :: forall t m.MonadWidget t m => HeistWidgetConfig t -> m (HeistWidget)
heistWidget (HeistWidgetConfig m0 dM ) = do

    -- Attach template listing to code editor
    rec (MarkupList k allMarkup) <- markupList $ MarkupListConfig m0 mUpdates defDrawEntry
        mCode <- markupEditor (M.lookup <$> k <*> allMarkup)
        let mUpdates = attachWith (=:) (current k) (Just <$> mCode)

    let parsedDocs :: Dynamic t ([String], [(T.Text, H.DocumentFile)]) = parseMarkups <$> allMarkup



    let spliceHoles = collectHoles spliceHoleParser . (fmap (X.docContent . H.dfDoc . snd) . snd) <$> parsedDocs

        previewName = (maybe "" _mcName) <$> (M.lookup <$> k <*> allMarkup)

    splices <- spliceWidgets spliceHoles

    hState <- heistState (H.emptyHeistConfig
                          & H.hcInterpretedSplices .~ H.defaultInterpretedSplices
                          & H.hcNamespace .~ ""
                         ) (snd <$> parsedDocs) splices

    preview previewName hState allMarkup

    return HeistWidget
