{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectCreate.View where

import Lucid
import Common.Web.Attributes
import Common.Web.Template.MainHeader (mainHeaderTemplate)
import Control.Monad                  (forM_, unless)
import Data.Maybe                     (fromMaybe)
import Data.Text                      (Text)
import Network.HTTP.Types             (status200)
import Network.Wai                    (Response, responseLBS, ResponseReceived)

data AddProjectPayload = AddProjectPayload
  { description :: Maybe Text
  , title       :: Maybe Text
  }

emptyPayload :: AddProjectPayload
emptyPayload = AddProjectPayload
  { description = Nothing
  , title       = Nothing
  }

handleProjectCreateVw :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleProjectCreateVw respond = do
  respond $ responseLBS
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    (renderBS $ projectCreateVwTemplate emptyPayload mempty)

projectCreateVwTemplate :: AddProjectPayload -> [Text] -> Html ()
projectCreateVwTemplate payload errs = do
  mainHeaderTemplate "Add Project" 
  link_ [rel_ "stylesheet", href_ "/static/styles/views/add-project.css"]
  div_  [class_ "view"] $ do
    form_ 
      [ id_ "form-create-project"
      , class_ "form-basic"
      , hxPost_ "/ui/create-project/submit"
      ] $ do
      span_ $ do
        label_ [for_ "title"] "Title:"
        input_ [type_ "text", name_ "title", value_ ttl]
      span_ $ do
        label_    [for_ "description"] "Description:"
        textarea_ [name_ "description"] (toHtml descr)
      span_ $ do
        button_
          [ class_ "action-button"
          , type_  "submit"
          ] "Submit"
      unless (null errs) $ do
        div_ [class_ "error-messages"] $ do
          forM_ errs $ p_ [class_ "error-message"] . toHtml
  where
    descr = fromMaybe mempty $ description payload 
    ttl   = fromMaybe mempty $ title payload 

