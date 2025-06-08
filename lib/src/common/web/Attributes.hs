{-# LANGUAGE OverloadedStrings #-}

module Common.Web.Attributes where

import Data.Text (Text)
import Lucid.Base (Attributes, makeAttributes, termRaw, TermRaw)

script' :: TermRaw arg result => arg -> result
script' a = termRaw "script" a

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
