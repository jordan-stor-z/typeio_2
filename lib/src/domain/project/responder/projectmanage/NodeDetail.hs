{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.ProjectManage.NodeDetail where

import Common.Either             (listToEither, maybeToEither)
import Common.Validation         ( (.$)
                                  , isThere 
                                  , isNotEmpty 
                                  , runValidation
                                  , ValidationErr
                                  , valRead
                                  )
import Common.Web.Query           (lookupVal)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Either (hoistEither, runEitherT)
import Control.Monad.Reader       (ReaderT)
import Data.Aeson                 (encode, object, (.=))
import Data.Maybe                 (listToMaybe)
import Data.Int                   (Int64)
import Data.Text                  (Text, pack, unpack)
import Data.Time                  (UTCTime)
import Database.Esqueleto.Experimental ( (==.)
                               , from
                               , limit
                               , select
                               , table
                               , val
                               , where_
                               )
import Database.Persist        (Entity(..))
import Database.Persist.Sql    (ConnectionPool, fromSqlKey, runSqlPool, SqlBackend, toSqlKey)
import Lucid                   (renderBS)
import Network.HTTP.Types      (status200, status403)
import Network.Wai             (Application, queryString, responseLBS)
import Network.HTTP.Types.URI  (QueryText, queryToQueryText)
import qualified Domain.Project.Model as M

data NodeDetailError = 
  InvalidParams [ValidationErr]
  | NodeNotFound

data Node = Node
  { nodeId          :: Int64
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

validateForm :: GetNodeDetailForm -> Either NodeDetailError GetNodeDetailPayload
validateForm fm = runValidation InvalidParams $ do
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

queryToForm :: QueryText -> GetNodeDetailForm
queryToForm qt = GetNodeDetailForm
  { formProjectId = lookupVal "projectId" qt
  , formNodeId    = lookupVal "nodeId" qt
  }

handleProjectManageView :: ConnectionPool -> Application
handleProjectManageView pl req respond = do
  let fm = queryToForm 
         . queryToQueryText 
         . queryString 
         $ req
  res <- runEitherT $ do
       py <- hoistEither $ validateForm fm
       nd <- lift 
             . flip runSqlPool pl 
             . queryNode 
             . payloadNodeId 
             $ py
       hoistEither nd 
  case res of
    Left (InvalidParams es) -> respondBadProjectId (pack . unlines . fmap unpack $ es)
    Left NodeNotFound       -> respondBadProjectId "Node not found"
    Right py -> do
      respond $ responseLBS
        status200 
        [("Content-Type", "text/html")]
        (renderBS undefined)
  where
    respondBadProjectId er = respond $ responseLBS
      status403
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= ("Invalid project ID\n" <> er :: Text)])

toNodeSchema :: Entity M.Node -> Node
toNodeSchema (Entity k e) = Node 
  { nodeId          = fromSqlKey k
  , nodeTitle       = pack . M.nodeTitle $ e
  , nodeDescription = pack . M.nodeDescription $ e
  , nodeStatusId    = pack . M.unNodeStatusKey . M.nodeNodeStatusId $ e
  , nodeTypeId      = pack . M.unNodeTypeKey . M.nodeNodeTypeId $ e
  , nodeUpdated     = M.nodeUpdated e
  }

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
