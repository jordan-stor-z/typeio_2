{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.Ui.ProjectManage.Node where

import Common.Web.Attributes
import Domain.Project.Responder.Ui.ProjectManage.Link
import Lucid 
import Common.Web.Query          (lookupVal)
import Common.Validation         ( (.$)
                                 , isNotEmpty
                                 , isThere
                                 , runValidation
                                 , ValidationErr
                                 , valRead
                                 )
import Data.Int                  (Int64)
import Data.Text                 (Text, unpack)
import Network.HTTP.Types.Status (status200, status400)
import Network.HTTP.Types.URI    (QueryText, queryToQueryText)
import Network.Wai               (Application, queryString, responseLBS)
import Data.Text.Util (intToText)

data GetNodePanelForm = GetNodePanelForm
  { formNodeId    :: Maybe Text
  , formProjectId :: Maybe Text
  } 

data GetNodePanelPayload = GetNodePanelPayload
  { payloadNodeId    :: Int64
  , payloadProjectId :: Int64
  }

handleGetNodePanel :: Application
handleGetNodePanel req respond = do
  case pyld of
    Left _ -> 
      respond 
      . responseLBS 
        status400 [] 
      $ "Error"
    Right payload -> 
      respond 
      . responseLBS 
        status200 [] 
      . renderBS
      . templateNodePanel (payloadNodeId payload) 
      $ payloadProjectId payload
  where
    pyld = validateForm 
           . queryTextToForm 
           . queryToQueryText 
           . queryString 
           $ req

queryTextToForm :: QueryText -> GetNodePanelForm
queryTextToForm qt = GetNodePanelForm 
  { formProjectId = lookupVal "projectId" qt
  , formNodeId    = lookupVal "nodeId"    qt
  }

templateNodePanel :: Int64 -> Int64 -> Html ()
templateNodePanel nid pid = do
  div_ [ class_ "panel-actions"
       , h_ $ "init add .node-highlight to #node-"
              <> intToText nid
              <> " on htmx:beforeCleanupElement remove .node-highlight from #node-"
              <> intToText nid
       ] $ do
   button_ [ class_    "edit-button pill-button"
           , hxGet_     $ editLink nid pid 
           , hxPushUrl_ False 
           , hxSwap_    "innerHTML"
           , hxTarget_  "#node-detail"
           , hxTrigger_ "click"
           , h_ "on htmx:afterOnLoad toggle .removed on .edit-button"
           ] $ i_  [class_ "material-icons"] "mode_edit"
   button_ [ class_    "edit-button pill-button removed selected"
           , hxGet_     $ nodeDetailLink nid pid 
           , hxPushUrl_ False 
           , hxSwap_    "innerHTML"
           , hxTarget_  "#node-detail"
           , hxTrigger_ "click"
           , h_ $ "on htmx:afterOnLoad" 
                  <> " toggle .removed on .edit-button"
                  <> " then trigger nodePanel:onEditClosed(nodeId:"
                  <> intToText nid
                  <> ")"
           ] $ i_  [class_ "material-icons"] "mode_edit"
   button_ [ class_      "pill-button"
           , hxGet_      "/ui/central/empty"
           , hxPushUrl'_ $ projectLink pid
           , hxSwap_     "innerHTML"
           , hxTarget_   "#node-panel"
           , hxTrigger_  "click"
           ] $ i_  [class_ "material-icons"] "close"
  div_ [ id_ "node-detail"
        , hxGet_ $ nodeDetailLink nid pid
        , hxPushUrl_ False
        , hxSwap_ "innerHTML"
        , hxTarget_ "#node-detail"
        , hxTrigger_ "load"
       ] empty
  where
    empty = mempty :: Html ()

validateForm :: GetNodePanelForm -> Either [ValidationErr] GetNodePanelPayload 
validateForm fm = runValidation id $ do
  pid <- formProjectId fm 
    .$ unpack
    >>= isThere    "Project id must be present"
    >>= isNotEmpty "Project id must have a value"
    >>= valRead    "Project id must be valid integer"
  nid <- formNodeId fm
    .$ unpack
    >>= isThere    "Node id must be present"
    >>= isNotEmpty "Node id must have a value"
    >>= valRead    "Node id must be valid integer"
  return $ GetNodePanelPayload <$> nid <*> pid

