{-# LANGUAGE OverloadedStrings #-}

module Common.Web.Attributes where

import Data.Aeson (encode, ToJSON, (.=), object)
import Data.Aeson.Key (fromText)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Encoding (decodeUtf8)
import Data.Text  (Text)
import Data.Text.Lazy (toStrict)
import Lucid.Base (Attributes, makeAttributes)

hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

hxInclude_ :: Text -> Attributes
hxInclude_ = makeAttributes "hx-include"

hxIndicator_ :: Text -> Attributes
hxIndicator_ = makeAttributes "hx-indicator"

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

hxPushUrl_ :: Bool -> Attributes
hxPushUrl_ = makeAttributes "hx-push-url" . boolText

hxPushUrl'_ :: Text -> Attributes
hxPushUrl'_ = makeAttributes "hx-push-url"

hxReplaceUrl_ :: Bool -> Attributes
hxReplaceUrl_ = makeAttributes "hx-replace-url" . boolText

hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

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
