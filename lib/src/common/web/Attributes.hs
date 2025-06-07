{-# LANGUAGE OverloadedStrings #-}

module Common.Web.Attributes where

import Data.Text (Text)
import Lucid.Base (Attributes, makeAttributes, HtmlT, termRaw, TermRaw, makeElementNoEnd)

script'_ :: Monad m => [Attributes] -> HtmlT m () 
script'_ = makeElementNoEnd "script" 

hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

hxPushUrl_ :: Bool -> Attributes
hxPushUrl_ = makeAttributes "hx-push-url" . boolText

hxReplaceUrl_ :: Bool -> Attributes
hxReplaceUrl_ = makeAttributes "hx-replace-url" . boolText

hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

hxTrigger_ :: Text -> Attributes
hxTrigger_ = makeAttributes "hx-trigger"

boolText :: Bool -> Text
boolText True  = "true"
boolText False = "false"
