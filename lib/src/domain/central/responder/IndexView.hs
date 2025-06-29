{-# LANGUAGE OverloadedStrings #-}

module Domain.Central.Responder.IndexView where

import Lucid 
import Common.Web.Attributes
import Data.Text          (Text)
import Network.HTTP.Types (status200)
import Network.Wai        (Response, responseLBS, ResponseReceived)

handleIndexView :: Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleIndexView path res = do 
  res $ responseLBS
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    (renderBS . indexTemplate $ path)

indexTemplate :: Text -> Html ()
indexTemplate path = html_ $ do
  let l = mempty :: Html ()
  head_ $ do
    title_   "TypeIO"
    link_    [rel_ "stylesheet", href_ "/static/styles/global.css"]
    meta_    [name_ "htmx-config", content_ "{\"historyCacheSize\": 0}"]
    script_  [src_ "https://unpkg.com/htmx.org@2.0.4"] l :: Html ()
  body_ $ do
    div_ 
      [ id_           "container"
      , hxGet_        path
      , hxTrigger_    "load"
      , hxReplaceUrl_ True
      , hxSwap_       "innerHTML"
      ] $ do 
        p_ "loading..."
