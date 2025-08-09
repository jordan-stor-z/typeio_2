{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.Node.ActionMenu where

import Domain.Project.Responder.ProjectManage.Link
import Domain.Project.Responder.ProjectManage.Node.Query
import Domain.Project.Responder.ProjectManage.Node.Validation
import Lucid
import Common.Validation               ( (.$)
                                       , isThere 
                                       , isNotEmpty 
                                       , runValidation
                                       , ValidationErr
                                       , valRead
                                       )
import Common.Web.Attributes
import Common.Web.Query                (lookupVal)
import Control.Monad                   (forM_, unless)
import Control.Monad.Reader            (ReaderT)
import Control.Monad.Trans.Class       (lift)
import Control.Monad.Trans.Either      (hoistEither, hoistMaybe, firstEitherT, runEitherT, EitherT)
import Data.Aeson                      ((.=) , object)
import Data.Int                        (Int64)
import Data.Text                       (Text, pack, unpack)
import Data.Text.Util                  (intToText)
import Data.Time                       (UTCTime)
import Data.Time.Format                (defaultTimeLocale, formatTime) 
import Database.Esqueleto.Experimental (from, select, table)
import Database.Persist                (Entity(..))
import Database.Persist.Sql            (ConnectionPool, fromSqlKey, runSqlPool, SqlBackend)
import Network.HTTP.Types              (status200, status400, status422, status500)
import Network.Wai                     (Application, queryString, responseLBS, Request, Response, ResponseReceived)
import Network.HTTP.Types.URI          (QueryText, queryToQueryText)
import qualified Domain.Project.Model as M

data ActionMenuForm = ActionMenuForm 
  { formNodeId    :: Maybe Text
  , formProjectId :: Maybe Text
  }

data ActionMenuPayload = ActionMenuPayload
  { payloadNodeId    :: Int64
  , payloadProjectId :: Int64
  }

handleNodeDetailActionMenu :: Application 
handleNodeDetailActionMenu req respond = do
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
      . templateNodeDetailActionMenu (payloadNodeId payload) 
      $ payloadProjectId payload
  where
    pyld = validateForm 
           . queryTextToForm 
           . queryToQueryText 
           . queryString 
           $ req

handleNodeEditActionMenu :: Application 
handleNodeEditActionMenu req respond = do
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
      . templateNodeEditActionMenu (payloadNodeId payload) 
      $ payloadProjectId payload
  where
    pyld = validateForm 
           . queryTextToForm 
           . queryToQueryText 
           . queryString 
           $ req


queryTextToForm :: QueryText -> ActionMenuForm 
queryTextToForm qt = ActionMenuForm 
  { formProjectId = lookupVal "projectId" qt
  , formNodeId    = lookupVal "nodeId"    qt
  }

templateNodeDetailActionMenu :: Int64 -> Int64 -> Html ()
templateNodeDetailActionMenu nid pid = do
  button_ [ class_    "edit-button pill-button"
          , hxGet_     $ editLink nid pid 
          , hxPushUrl_ False 
          , hxSwap_    "innerHTML"
          , hxTarget_  "#node-detail"
          , hxTrigger_ "click"
          ] $ i_  [class_ "material-icons"] "mode_edit"
  button_ [ id_ "btn-close"
           , class_      "pill-button"
           , hxGet_      "/ui/central/empty"
           , hxPushUrl'_ $ projectLink pid
           , hxSwap_     "innerHTML"
           , hxTarget_   "#node-panel"
           , hxTrigger_  "click"
           ] $ i_  [class_ "material-icons"] "close"

templateNodeEditActionMenu :: Int64 -> Int64 -> Html ()
templateNodeEditActionMenu nid pid = do
  button_ [ class_    "edit-button pill-button selected"
          , hxGet_     $ nodeDetailLink nid pid 
          , hxPushUrl_ False 
          , hxSwap_    "innerHTML"
          , hxTarget_  "#node-detail"
          , hxTrigger_ "click"
          ] $ i_  [class_ "material-icons"] "mode_edit"

validateForm :: ActionMenuForm -> Either [ValidationErr] ActionMenuPayload 
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
  return $ ActionMenuPayload <$> nid <*> pid

