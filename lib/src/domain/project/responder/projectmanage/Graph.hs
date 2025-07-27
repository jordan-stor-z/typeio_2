{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.ProjectManage.Graph where

import Common.Web.Attributes
import Domain.Project.Responder.ProjectManage.Link
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
import Control.Monad.Trans.Either (hoistEither, runEitherT)
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
  , push        :: Text
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
          , link        = nodePanelLink (nodeId n) (nodeProjectId n)
          , push        = pushUrl (nodeId n) (nodeProjectId n)
          }

handleProjectGraph :: ConnectionPool ->  Application
handleProjectGraph pl req respond = do
  rslt <- flip runSqlPool pl . runEitherT $ do
    pid <- hoistEither 
           . first InvalidParams 
           . validateProjectId 
           $ qt
    ns  <- lift (queryNodes pid)
           >>= hoistEither 
               . notNullEither MissingNodes
    ds  <- lift 
           . queryDependencies 
           . fmap nodeId 
           $ ns
    pure . buildGraph ns $ ds 
  case rslt of
    Left (InvalidParams es) -> respondValErrs es
    Left MissingNodes       -> respondMissingNodes
    Right graph             -> respondSuccess graph
  where
    respondMissingNodes = 
      respond
      . responseLBS
        status403
        [("Content-Type", "application/json")]
      . encode 
      . object 
      $ ["error" .= ("No nodes found for the project" :: Text)]
    respondValErrs es = 
      respond 
      . responseLBS
        status403
        [("Content-Type", "application/json")]
      . encode
      . object
      $ ["error" .= (mconcat . map (pack . show) $ es)]
    respondSuccess = 
      respond 
      . responseLBS
        status200
        [("Content-Type", "text/html")]
      . renderBS
      . templateGraph
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
queryDependencies []   = return []
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
  script_ [id_ "graph-data", type_ "application/json"] gd
  script_ [src_ "/static/script/nodetree.js"]          empty 
  svg_    [ id_     "tree-view"
          , height_ "100%"
          , width_  "100%"
          , h_ "on load transition my opacity to 1 over 200ms"
          ] empty 
  div_ [class_ "hidden", h_ "on node:titleUpdated from #node-panel log event"] empty
  where 
    empty = mempty :: Html ()
    gd    = encode g

validateProjectId ::  QueryText -> Either [ValidationErr] Int64
validateProjectId qt = runValidation id $ do
  lookupVal "projectId" qt 
    .$ unpack
    >>= isThere    "Project id must be present" 
    >>= isNotEmpty "Project id must have a value"
    >>= valRead    "Project id must be valid integer" 
