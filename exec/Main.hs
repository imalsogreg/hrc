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

-------------------------------------------------------------------------------
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
import           Reflex.Dom                             hiding (run)
import           Text.Read                              (readMaybe)
import qualified Text.XmlHtml                           as X
-------------------------------------------------------------------------------
import qualified Reflex.Heist.FullWidget                as FW
import           Reflex.Heist.Markup                    (MarkupCode(..))


-------------------------------------------------------------------------------
main :: IO ()
main = mainWidget run


-------------------------------------------------------------------------------
run :: forall t m. MonadWidget t m => m ()
run = do
    text "1"
    FW.heistWidget $ FW.HeistWidgetConfig templates1 never
    return ()


-------------------------------------------------------------------------------
templates1 :: M.Map Int MarkupCode
templates1 =
       1 =: MarkupCode "block" "src"  (T.decodeUtf8 testBase)
    <> 2 =: MarkupCode "test1" "src" (T.decodeUtf8 testTemplate1)
    <> 3 =: MarkupCode "fancy" "src" (T.decodeUtf8 testTemplate2)

templates2 :: M.Map Int MarkupCode
templates2 = 1 =: MarkupCode "test" "src" "<h1>Hi</h1>"

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

Hi <splice:name splicetype="text"/>

<apply template="block">
Your next <apply template="fancydrink"/>
earns <splice:numStars splicetype="double"/> Stars!
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
<span class="aspan">
  <em>
    <splice:thing splicetype="dropdown" options="ThingA,thinga;ThingB,thingb;ThingC,thingc"/>
  </em>
</span>

<style>
.drink {
  text-shadow: 0px 0px 5px rgba(0,255,250,1);
  color: black;
}
</style>
|]


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


listingItemAttrs :: Bool -> M.Map T.Text T.Text
listingItemAttrs sel =
    let brd = bool "" " border-left: 3px solid black;" sel
    in  "style" =: ("display:flex; align-items:center; background-color: rgba(0,0,0,0.1); padding: 10px;" <> brd)
