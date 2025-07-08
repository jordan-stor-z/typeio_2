{-# LANGUAGE OverloadedStrings #-}

module Common.Web.Query where

import Data.List              (find)
import Data.Maybe             (fromMaybe)
import Data.Text              (Text)
import Network.HTTP.Types.URI (QueryText)

lookupVal :: Text -> QueryText -> Maybe Text 
lookupVal k pr = find ((== k) . fst) pr >>= snd
 
queryTextToText :: QueryText -> Maybe Text
queryTextToText [] = Nothing
queryTextToText qs = Just $ foldr (\(k, v) acc -> 
  acc <> k <> "=" <> fromMaybe "" v <> "&") "?" qs
