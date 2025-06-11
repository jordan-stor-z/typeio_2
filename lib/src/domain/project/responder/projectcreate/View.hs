{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectCreate.View where

import Lucid
import Common.Web.Attributes
import Data.Text          (Text)
import Network.HTTP.Types (status200)
import Network.Wai        (Response, responseLBS, ResponseReceived)

data AddProjectPayload = AddProjectPayload
  { description :: Text
  , title       :: Text
  }

handleProjectCreateVw :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleProjectCreateVw respond = do
  respond $ responseLBS
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    (renderBS $ projectCreateVwTemplate Nothing)

projectCreateVwTemplate :: Maybe AddProjectPayload -> Html ()
projectCreateVwTemplate payload = do
  div_ [class_ "view"] $ do
    form_ 
      [ id_ "form-create-project"
      , class_ "form-basic"
      , hxPost_ "/ui/project-add/create"
      , hxSwap_ "innerHTML"
      , hxTarget_ "#container"
      ] $ do
      span_ $ do
        label_ [for_ "title"] "Title:"
        input_ [type_ "text", name_ "title", value_ ttl]
      span_ $ do
        label_ [for_ "description"] "Description:"
        input_ [type_ "text", name_ "description", value_ descr]
      span_ $ do
        button_
          [ class_ "action-button"
          , type_  "submit"
          ] "Submit"
  where
    descr = maybe mempty description payload 
    ttl   = maybe mempty title payload

