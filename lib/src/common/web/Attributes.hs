{-# LANGUAGE OverloadedStrings #-}

module Common.Web.Attributes where

import Data.Aeson (ToJSON, (.=), object)
import Data.Aeson.Key (fromText)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text  (Text)
import Data.Text.Lazy (toStrict)
import Lucid.Base (Attributes, makeAttributes)

h_ :: Text -> Attributes
h_ = makeAttributes "_"

hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

hxInclude_ :: Text -> Attributes
hxInclude_ = makeAttributes "hx-include"

hxIndicator_ :: Text -> Attributes
hxIndicator_ = makeAttributes "hx-indicator"

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

hxPut_ :: Text -> Attributes
hxPut_ = makeAttributes "hx-put"

hxPushUrl_ :: Bool -> Attributes
hxPushUrl_ = makeAttributes "hx-push-url" . boolText

hxPushUrl'_ :: Text -> Attributes
hxPushUrl'_ = makeAttributes "hx-push-url"

hxReplaceUrl_ :: Bool -> Attributes
hxReplaceUrl_ = makeAttributes "hx-replace-url" . boolText

hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

hxSync_ :: Text -> Attributes
hxSync_ = makeAttributes "hx-sync"

hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

hxTrigger_ :: Text -> Attributes
hxTrigger_ = makeAttributes "hx-trigger"

hxVals_ :: [(Text, Text)] -> Attributes
hxVals_ = makeAttributes "hx-vals" 
  . toStrict 
  . encodeToLazyText 
  . object 
  . fmap (\(k, v) -> fromText k .= v)

hxVals'_ :: ToJSON a => a -> Attributes
hxVals'_ = makeAttributes "hx-vals" 
          . toStrict 
          . encodeToLazyText

boolText :: Bool -> Text
boolText True  = "true"
boolText False = "false"
