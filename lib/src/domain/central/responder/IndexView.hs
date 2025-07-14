{-# LANGUAGE OverloadedStrings #-}

module Domain.Central.Responder.IndexView where

import Lucid 
import Common.Web.Attributes
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List  (intersperse)
import Data.Text  (Text)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.URI         (QueryText, queryToQueryText)
import Network.Wai        (Application, queryString, Response, responseLBS, ResponseReceived)

queryTextToText :: QueryText -> Maybe Text
queryTextToText [] = Nothing
queryTextToText qs = Just $ foldr (\(k, v) acc -> 
  acc <> k <> "=" <> fromMaybe "" v <> "&") "?" qs

handleIndexView :: Text -> Application 
handleIndexView path req res = do 
  res $ responseLBS
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    (renderBS . indexTemplate path $ qs)
  where 
    qs = queryTextToText . queryToQueryText . queryString $ req

indexTemplate :: Text -> Maybe Text -> Html ()
indexTemplate path qs = html_ $ do
  let l   = mempty :: Html ()
      lnk = maybe path (path <>) qs
  head_ $ do
    title_   "TypeIO"
    link_    [rel_ "stylesheet", href_ "/static/styles/global.css"]
    link_    [ rel_ "stylesheet"
             , href_ "https://fonts.googleapis.com/icon?family=Material+Icons"
             ]
    meta_    [name_ "htmx-config", content_ "{\"historyCacheSize\": 0}"]
    script_  [src_ "https://unpkg.com/htmx.org@2.0.4"] l :: Html ()
  body_ $ do
    div_ 
      [ id_           "container"
      , hxGet_        lnk 
      , hxTrigger_    "load"
      , hxReplaceUrl_ True
      , hxSwap_       "innerHTML"
      ] $ do 
        p_ "loading..."
