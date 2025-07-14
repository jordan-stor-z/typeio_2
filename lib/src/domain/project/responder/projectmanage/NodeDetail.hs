{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.ProjectManage.NodeDetail where

import Lucid
import Common.Either                   (listToEither)
import Common.Validation               ( (.$)
                                       , isEq
                                       , isThere 
                                       , isNotEmpty 
                                       , runValidation
                                       , ValidationErr
                                       , valRead
                                       )
import Common.Web.Attributes
import Common.Web.Query                (lookupVal, queryTextToText)
import Control.Monad                   (forM_, unless)
import Control.Monad.Reader            (ReaderT)
import Control.Monad.Trans.Class       (lift)
import Control.Monad.Trans.Either      (hoistEither, newEitherT, runEitherT, EitherT)
import Data.Bifunctor                  (first)
import Data.Int                        (Int64)
import Data.Text                       (Text, pack, unpack)
import Data.Time                       (UTCTime)
import Data.Time.Format                (defaultTimeLocale, formatTime) 
import Database.Esqueleto.Experimental ( (==.)
                                       , from
                                       , limit
                                       , select
                                       , table
                                       , val
                                       , where_
                                       )
import Database.Persist                (Entity(..))
import Database.Persist.Sql            (ConnectionPool, fromSqlKey, runSqlPool, SqlBackend, toSqlKey)
import Network.HTTP.Types              (status200)
import Network.Wai                     (Application, queryString, responseLBS)
import Network.HTTP.Types.URI          (QueryText, queryToQueryText)
import qualified Domain.Project.Model as M
import qualified Data.ByteString      as B (pack)

data NodeDetailError = 
  InvalidParams [ValidationErr]
  | NodeNotFound

data Node = Node
  { nodeId          :: Int64
  , nodeProjectId   :: Int64
  , nodeTitle       :: Text
  , nodeDescription :: Text
  , nodeStatusId    :: Text
  , nodeTypeId      :: Text 
  , nodeUpdated     :: UTCTime
  }

data GetNodeDetailForm = GetNodeDetailForm
  { formProjectId :: Maybe Text 
  , formNodeId    :: Maybe Text 
  }

data GetNodeDetailPayload = GetNodeDetailPayload
  { payloadProjectId :: Int64
  , payloadNodeId    :: Int64
  }

handleGetNodeDetail :: ConnectionPool -> Application
handleGetNodeDetail pl req respond = do
  res <- runEitherT $ do
       py  <- hoistEither 
            . first InvalidParams 
            . validateForm 
            $ form
       nd' <- lift 
             . flip runSqlPool pl 
             . queryNode 
             . payloadNodeId 
             $ py
       nd  <- hoistEither nd'
       hoistEither 
        . first InvalidParams 
        . validateNodeProjectId py 
        $ nd
  case res of
    Left (InvalidParams es) -> do
      respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        (renderBS . templateInvalidParams $ es)
    Left NodeNotFound -> do 
      respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        (renderBS templateNodeNotFound)
    Right nd -> do
      respond $ responseLBS
        status200 
        [("Content-Type", "text-html")]
        (renderBS . templateNode $ nd)
    where
      form = queryTextToForm 
             . queryToQueryText 
             . queryString 
             $ req 

queryNode :: Int64 -> ReaderT SqlBackend IO (Either NodeDetailError Node)
queryNode nid = do
  ns <-  select $ do
    n <- from $ table @M.Node
    where_ (n.id ==. val nkey)
    limit 1
    pure n
  return 
    . listToEither NodeNotFound 
    . fmap toNodeSchema 
    $ ns
  where 
    nkey = toSqlKey @M.Node nid 
    toNodeSchema (Entity k e) = Node 
      { nodeId          = fromSqlKey k
      , nodeProjectId   = fromSqlKey . M.nodeProjectId $ e
      , nodeTitle       = pack . M.nodeTitle $ e
      , nodeDescription = pack . M.nodeDescription $ e
      , nodeStatusId    = pack . M.unNodeStatusKey . M.nodeNodeStatusId $ e
      , nodeTypeId      = pack . M.unNodeTypeKey . M.nodeNodeTypeId $ e
      , nodeUpdated     = M.nodeUpdated e
      }

queryTextToForm :: QueryText -> GetNodeDetailForm
queryTextToForm qt = GetNodeDetailForm
  { formProjectId = lookupVal "projectId" qt
  , formNodeId    = lookupVal "nodeId" qt
  }

templateNodeNotFound :: Html ()
templateNodeNotFound = do
  div_ [] "Node not found" 

templateInvalidParams :: [ValidationErr] -> Html ()
templateInvalidParams es = do
  div_ []  $ do
    unless (null es) $ do
      div_ [class_ "error-messages"] $ do
        forM_ es $ p_ [class_ "error-message"] . toHtml

templateNode :: Node -> Html ()
templateNode nd = do
    header_ [class_ "node-header"] $ do
      h1_ [] (toHtml . nodeTitle $ nd)
      i_ [ class_ "material-icons close-icon"
           , hxGet_ (projectLink . nodeProjectId $ nd) 
           , hxPushUrl_ True
           , hxSwap_ "innerHTML"
           , hxTarget_ "#container"
           , hxTrigger_ "click"
         ] "close"
    section_ [class_ "node-description"] $ do
      p_ [] (toHtml . nodeDescription $ nd)
    section_ [class_ "node-properties"] $ do
      div_ [class_ "property-item"] $ do
        span_ [class_ "property-label"] "Status:"
        span_ [class_ "property-value"] (toHtml . nodeStatusId $ nd)
      div_ [class_ "property-item"] $ do
        span_ [class_ "property-label"] "Type:"
        span_ [class_ "property-value"] (toHtml . showNodeType . nodeTypeId $ nd)
      div_ [class_ "property-item"] $ do
        span_ [class_ "property-label"] "Last Updated:"
        span_ [class_ "property-value"] (toHtml . formatUpdated . nodeUpdated $ nd)
  where
    projectLink = (<>) "/ui/project/vw?projectId=" . pack . show

formatUpdated :: UTCTime -> Text
formatUpdated = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" 

showNodeType :: Text -> Text
showNodeType typ = case typ of
  "project_root" -> "Root"
  "work"         -> "Work"
  _              -> typ

validateForm :: GetNodeDetailForm -> Either [ValidationErr]  GetNodeDetailPayload
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
  return $ GetNodeDetailPayload <$> pid <*> nid 

validateNodeProjectId :: GetNodeDetailPayload 
  -> Node 
  -> Either [ValidationErr] Node
validateNodeProjectId fm nd = runValidation id $ do
  _ <- Just nd
    .$ nodeProjectId 
    >>= isEq (payloadProjectId fm) "Invalid state. Node is not part of project"
  return . Just $ nd

