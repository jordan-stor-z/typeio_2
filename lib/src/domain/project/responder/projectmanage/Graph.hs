{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.ProjectManage.Graph where

import Lucid
import Common.Either              (notNullEither)
import Common.Validation          ( (.$)
                                  , isNotEmpty
                                  , isThere
                                  , runValidation
                                  , ValidationErr
                                  , valRead
                                  )
import Common.Web.Query           (lookupVal)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Either (hoistEither, newEitherT, runEitherT)
import Control.Monad.Reader       (ReaderT)
import Data.Aeson                 (encode, ToJSON(..), (.=), object)
import Data.Int                   (Int64)
import Data.Bifunctor             (first)
import Data.Text                  (pack, Text, unpack)
import Data.Time.Clock            (UTCTime)
import Database.Esqueleto.Experimental ( from
                                       , fromSqlKey
                                       , select
                                       , table
                                       , where_
                                       , (==.)
                                       , toSqlKey
                                       , in_
                                       , val
                                       , valList
                                       )
import Database.Persist               (Entity(..))
import Database.Persist.Sql           (ConnectionPool, SqlBackend, runSqlPool)
import Network.HTTP.Types             (status200, status403, queryToQueryText)
import Network.HTTP.Types.URI         (QueryText)
import Network.Wai                    (Application, responseLBS, Request (queryString))
import qualified Domain.Project.Model as M ( Dependency(..)
                                           , Node(..)
                                           , Project(..)
                                           , unNodeStatusKey
                                           , unNodeTypeKey
                                           )

data GetGraphError = 
  InvalidParams [ValidationErr]
  | MissingDependencies
  | MissingNodes 

data Dependency = Dependency
  { dependencyId :: Int64
  , childNodeId  :: Int64
  , parentNodeId :: Int64
  }

data Node = Node
  { nodeId          :: Int64
  , nodeName        :: String
  , nodeDescription :: String
  , nodeProjectId   :: Int64
  , nodeStatusId    :: String
  , nodeTitle       :: String
  , nodeTypeId      :: String
  , nodeUpdated     :: UTCTime 
  }

nodeLink :: Int64 -> Int64 -> Text
nodeLink nid pid = "/ui/project/node" 
                   <> "?nodeId=" 
                   <> (pack . show $ nid)
                   <> "&projectId=" 
                   <> (pack . show $ pid)

pushUrl:: Int64 -> Int64 -> Text
pushUrl nid pid = "/ui/project/vw"
                   <> "?projectId=" 
                   <> (pack . show $ pid)
                   <> "&nodeId=" 
                   <> (pack . show $ nid)

data GraphNode = GraphNode 
  { graphNodeId :: Int64
  , label       :: Text
  , pinned      :: Bool
  , link        :: Text
  , push     :: Text
  }

data Graph = Graph
  { nodes :: [GraphNode]
  , links :: [GraphLink]
  }

instance ToJSON Graph where
  toJSON (Graph ns ls) =
    object [ "nodes" .= ns
           , "links" .= ls
           ]

instance ToJSON GraphNode where
  toJSON (GraphNode gid lbl pnd lnk psh) =
    object [ "id"     .= gid
           , "label"  .= lbl
           , "pinned" .= pnd
           , "link"   .= lnk 
           , "push" .= psh
           ]

data GraphLink = GraphLink
  { source :: Int64
  , target :: Int64
  }

instance ToJSON GraphLink where
  toJSON (GraphLink src tgt) =
    object [ "source" .= src
           , "target" .= tgt
           ]

type ProjectId = Text

buildGraph :: [Node] -> [Dependency] -> Graph 
buildGraph ns ds = Graph (map toGNode ns) (map toLink ds)
  where
    toLink d  = GraphLink (childNodeId d) (parentNodeId d)
    toGNode n = GraphNode 
          { graphNodeId = nodeId n
          , label       = pack $ nodeName n
          , pinned      = nodeTypeId n == "project_root" 
          , link        = nodeLink (nodeId n) (nodeProjectId n)
          , push        = pushUrl (nodeId n) (nodeProjectId n)
          }

handleProjectGraph :: ConnectionPool ->  Application
handleProjectGraph pl req respond = do
  res <- runEitherT $ do
    pid <- hoistEither 
           . first InvalidParams 
           . validateProjectId 
           $ qt
    (ns, ds) <- lift . flip runSqlPool pl $ do
      n <- queryNodes pid
      d <- queryDependencies $ fmap nodeId n
      pure (n, d)
    ns'  <- hoistEither . notNullEither MissingNodes $ ns
    ds'  <- hoistEither . notNullEither MissingDependencies $ ds
    return $ buildGraph ns' ds' 
  case res of
    Left _    -> do 
      respond $ responseLBS
        status403
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid project ID" :: Text)])
    Right graph -> do
      respond $ responseLBS
        status200 
        [("Content-Type", "text/html")]
        (renderBS $ templateGraph graph)
  where
    qt = queryToQueryText 
         . queryString 
         $ req

queryNodes :: Int64 -> ReaderT SqlBackend IO [Node]
queryNodes pid = do
  ns <- select $ do
    n <- from $ table @M.Node
    where_ (n.projectId ==. val pkey)
    pure n
  return $ toNodeSchema <$> ns
  where 
    pkey = toSqlKey @M.Project pid 
    toNodeSchema (Entity k e) = Node 
      { nodeId          = fromSqlKey k
      , nodeName        = M.nodeTitle e
      , nodeDescription = M.nodeDescription e
      , nodeProjectId   = fromSqlKey . M.nodeProjectId $ e
      , nodeStatusId    = M.unNodeStatusKey . M.nodeNodeStatusId $ e
      , nodeTitle       = M.nodeTitle e
      , nodeTypeId      = M.unNodeTypeKey . M.nodeNodeTypeId $ e
      , nodeUpdated     = M.nodeUpdated e
      }

queryDependencies :: [Int64] -> ReaderT SqlBackend IO [Dependency]
queryDependencies nids = do
  let nkeys = toSqlKey @M.Node <$> nids
  ds <- select $ do
    d <- from $ table @M.Dependency
    where_ (d.nodeId `in_` valList nkeys)
    pure d
  return $ toDependencySchema <$> ds
  where
    toDependencySchema (Entity k d) = 
      Dependency 
        { dependencyId = fromSqlKey k
        , childNodeId  = fromSqlKey . M.dependencyNodeId $ d 
        , parentNodeId = fromSqlKey . M.dependencyToNodeId $ d 
        }

templateGraph :: Graph -> Html ()
templateGraph g = do
  let empty = mempty :: Html ()
  let js    = encode g
  script_ [id_ "graph-data", type_ "application/json"] js 
  script_ [src_ "/static/script/nodetree.js"]          empty 
  svg_    [ id_     "tree-view"
          , height_ "100%"
          , width_  "100%"
          ] empty 

validateProjectId ::  QueryText -> Either [ValidationErr] Int64
validateProjectId qt = runValidation id $ do
  lookupVal "projectId" qt 
    .$ unpack
    >>= isThere    "Project id must be present" 
    >>= isNotEmpty "Project id must have a value"
    >>= valRead    "Project id must be valid integer" 
