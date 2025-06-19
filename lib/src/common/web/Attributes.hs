{-# LANGUAGE OverloadedStrings #-}

module Common.Web.Attributes where

import Data.Text  (Text)
import Lucid.Base (Attributes, makeAttributes)

hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

hxPushUrl_ :: Bool -> Attributes
hxPushUrl_ = makeAttributes "hx-push-url" . boolText

hxReplaceUrl_ :: Bool -> Attributes
hxReplaceUrl_ = makeAttributes "hx-replace-url" . boolText

hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

hxTrigger_ :: Text -> Attributes
hxTrigger_ = makeAttributes "hx-trigger"

boolText :: Bool -> Text
boolText True  = "true"
boolText False = "false"
