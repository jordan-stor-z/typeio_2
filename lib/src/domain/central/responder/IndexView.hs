{-# LANGUAGE OverloadedStrings #-}

module Domain.Central.Responder.IndexView where

import Lucid 
import Lucid.Base (TermRaw(..))
import Common.Web.Attributes
import Common.Web.Types   (Responder)
import Data.Monoid (mempty)
import Data.Text          (Text)
import Network.HTTP.Types (status200)
import Network.Wai        (responseLBS)

handleIndexView :: Text -> Responder
handleIndexView path res = do 
  res $ responseLBS
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    (renderBS . indexTemplate $ path)

indexTemplate :: Text -> Html ()
indexTemplate path = html_ $ do
  let l = "something" :: Html ()
  head_ $ do
    title_  "TypeIO"
    link_    [rel_ "stylesheet", href_ "/static/styles/global.css"]
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
