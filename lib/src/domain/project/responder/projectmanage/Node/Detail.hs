{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.Node.Detail where

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
import Control.Monad.Trans.Class       (lift)
import Control.Monad.Trans.Either      (hoistEither, hoistMaybe, firstEitherT, runEitherT, EitherT)
import Data.Int                        (Int64)
import Data.Text                       (Text, pack, unpack)
import Data.Time                       (UTCTime)
import Data.Time.Format                (defaultTimeLocale, formatTime) 
import Database.Persist                (Entity(..))
import Database.Persist.Sql            (ConnectionPool, fromSqlKey, runSqlPool)
import Network.HTTP.Types              (status200)
import Network.Wai                     (queryString, responseLBS, Request, Response, ResponseReceived)
import Network.HTTP.Types.URI          (QueryText, queryToQueryText)
import qualified Domain.Project.Model as M

class NodeSchema a where
  schemaNodeId :: a -> Int64
  schemaProjectId :: a -> Int64
  schemaTitle :: a -> Text
  schemaDescription :: a -> Text
  schemaUpdated :: a -> UTCTime
  schemaStatusId :: a -> Text
  schemaTypeId :: a -> Text

data NodeDetailError = 
  InvalidParams [ValidationErr]
  | NodeNotFound

data Node = Node
  { nodeId      :: Int64
  , projectId   :: Int64
  , title       :: Text
  , description :: Text
  , updated     :: UTCTime
  , statusId    :: Text
  , typeId      :: Text 
  }

data GetNodeDetailForm = GetNodeDetailForm
  { formProjectId :: Maybe Text 
  , formNodeId    :: Maybe Text 
  }

data GetNodeDetailPayload = GetNodeDetailPayload
  { payloadProjectId :: Int64
  , payloadNodeId    :: Int64
  }

formatUpdated :: UTCTime -> Text
formatUpdated = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" 

handleErr :: NodeDetailError -> Response
handleErr er = case er of
    (InvalidParams es) ->
      responseLBS
          status200
          [("Content-Type", "text/html")]
        . renderBS . templateInvalidParams 
        $ es
    NodeNotFound -> do 
      responseLBS
          status200
          [("Content-Type", "text/html")]
        . renderBS 
        $ templateNodeNotFound

handleGetNodeDetail :: ConnectionPool
  -> Request 
  -> (Response -> IO ResponseReceived) 
  -> IO ResponseReceived
handleGetNodeDetail pl req respond = do
  rslt <- flip runSqlPool pl . runEitherT $ do
       pyld  <- firstEitherT InvalidParams 
            . validateForm 
            $ form
       lift (queryNode . payloadNodeId $ pyld)
          >>= hoistMaybe NodeNotFound
          >>= ( firstEitherT InvalidParams 
                . validateNodeProjectId (payloadProjectId pyld) 
              )
  case rslt of
    Left  e   -> respond . handleErr $ e
    Right nde -> respond 
                 . responseLBS
                   status200 
                   [("Content-Type", "text-html")]
                 . renderBS 
                 . templateNodeDetail 
                 . toNodeSchema
                 $ nde 
    where
      form = queryTextToForm 
             . queryToQueryText 
             . queryString 
             $ req

queryTextToForm :: QueryText -> GetNodeDetailForm
queryTextToForm qt = GetNodeDetailForm
  { formProjectId = lookupVal "projectId" qt
  , formNodeId    = lookupVal "nodeId" qt
  }

showNodeType :: Text -> Text
showNodeType typ = case typ of
  "project_root" -> "Root"
  "work"         -> "Work"
  _              -> typ

templateNodeNotFound :: Html ()
templateNodeNotFound = do
  div_ [] "Node not found" 

templateInvalidParams :: [ValidationErr] -> Html ()
templateInvalidParams es = do
  div_ []  $ do
    unless (null es) $ do
      div_ [class_ "error-messages"] $ do
        forM_ es $ p_ [class_ "error-message"] . toHtml

templateNodeDetail :: Node -> Html ()
templateNodeDetail nde = do
  header_ [] $
      h2_ [] (toHtml . title $ nde)
  section_ [] $
    p_ [] (toHtml . description $ nde)
  section_ [id_ "node-properties"] $ do
    article_ [] $ do
      span_ [class_ "property-label"] "Status:"
      span_ [class_ "property-value"] (toHtml . statusId $ nde)
    article_ [] $ do
      span_ [class_ "property-label"] "Type:"
      span_ [class_ "property-value"] (toHtml . showNodeType . typeId $ nde)
    article_ [] $ do
      span_ [class_ "property-label"] "Last Updated:"
      span_ [class_ "property-value"] (toHtml . formatUpdated . updated $ nde)

toNodeSchema :: Entity M.Node -> Node
toNodeSchema (Entity k e) = Node 
  { nodeId      = fromSqlKey k
  , description = pack . M.nodeDescription $ e
  , projectId   = fromSqlKey . M.nodeProjectId $ e
  , statusId    = pack . M.unNodeStatusKey . M.nodeNodeStatusId $ e
  , title       = pack . M.nodeTitle $ e
  , typeId      = pack . M.unNodeTypeKey . M.nodeNodeTypeId $ e
  , updated     = M.nodeUpdated e
  }

validateForm :: Monad m 
  => GetNodeDetailForm 
  -> EitherT [ValidationErr] m GetNodeDetailPayload
validateForm fm = hoistEither . runValidation id $ do
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

