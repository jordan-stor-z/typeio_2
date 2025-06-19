{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectIndex.View where

import Lucid
import Common.Web.Attributes
import Common.Web.Template.MainHeader (templateNavHeader)
import Network.HTTP.Types             (status200)
import Network.Wai                    (Response, responseLBS, ResponseReceived)

handleProjectView :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleProjectView respond = do
  respond $ responseLBS
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    (renderBS projectIndexVwTemplate)

projectIndexVwTemplate :: Html ()
projectIndexVwTemplate = do
    templateNavHeader "Projects" 
    div_ [class_ "view"] $ do
      button_ 
        [ class_     "action-button"
        , hxGet_     "/ui/create-project/vw"
        , hxPushUrl_ True 
        , hxTarget_  "#container"
        , hxSwap_    "innerHTML"
        ] "Create Project"
      div_ 
        [ hxGet_     "/ui/projects/list"
        , hxPushUrl_ False
        , hxTrigger_ "load"
        , hxSwap_    "innerHTML"
        ] mempty 


