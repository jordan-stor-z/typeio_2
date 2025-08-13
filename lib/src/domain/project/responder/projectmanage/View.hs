{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.View where

import Domain.Project.Responder.ProjectManage.Link
import Lucid
import Common.Validation              ((.$)
                                      , ValidationErr
                                      , isNotEmpty
                                      , isThere
                                      , runValidation
                                      , valRead
                                      )
import Common.Web.Attributes
import Common.Web.Template.MainHeader (templateNavHeader)
import Data.Int                       (Int64)
import Data.Text                      (Text, unpack)
import Network.HTTP.Types             (status200, status403, QueryText)
import Network.Wai                    (Application, queryString, responseLBS)
import Network.HTTP.Types.URI         (queryToQueryText)
import Common.Web.Query               (lookupVal)

data ManageProjectForm = ManageProjectForm 
  { formNodeId    :: Maybe Text
  , formProjectId :: Maybe Text 
  }

data ManageProjectPayload = ManageProjectPayload
  { payloadNodeId    :: Maybe Int64
  , payloadProjectId :: Int64
  }

handleProjectManageView :: Application
handleProjectManageView req respond = do
  case pidE of
    Left _   -> do
      respond $ responseLBS
        status403
        [("Content-Type", "text/html")]
        "Bad project id"
    Right py -> do
      respond $ responseLBS
        status200 
        [("Content-Type", "text/html")]
        (renderBS $ templateProject py)
  where
    pidE = validateForm 
          . queryTextToForm 
          . queryToQueryText 
          . queryString 
          $ req

queryTextToForm :: QueryText -> ManageProjectForm
queryTextToForm qt = ManageProjectForm
  { formNodeId = lookupVal "nodeId" qt
  , formProjectId = lookupVal "projectId" qt
  }

templateProject :: ManageProjectPayload -> Html ()
templateProject py = do
  templateNavHeader "Project"
  link_ [ rel_ "stylesheet"
        , href_ "/static/styles/views/manage-project.css"
        ]
  div_ [id_ "view"] $ do
    div_ [ id_ "tree-container" 
         , hxGet_     (graphLink pid)
         , hxPushUrl_ False
         , hxSwap_    "innerHTML"
         , hxTrigger_ "load"
         ] empty
    div_ [ id_ "node-panel" 
         ] empty
    case nidM of
      Nothing  -> empty
      Just nid -> do
        div_ [ class_    "hidden" 
             , hxGet_     (nodePanelLink nid pid)
             , hxPushUrl_ False
             , hxTarget_  "#node-panel"
             , hxTrigger_ "load"
             , hxSync_ "#tree-container:queue last"
             ] empty 
  where
    empty = mempty :: Html ()
    nidM  = payloadNodeId py
    pid   = payloadProjectId py

validateForm :: ManageProjectForm -> Either [ValidationErr] ManageProjectPayload
validateForm fm = runValidation id $ do
  pid <- formProjectId fm
    .$ unpack
    >>= isThere    "Project id is required"
    >>= isNotEmpty "Project id must have value"
    >>= valRead    "Project id must be valid integer"
  nid <- formNodeId fm
    .$ unpack
    >>= valRead "Node id must be valid integer"
  return $ ManageProjectPayload nid <$> pid


