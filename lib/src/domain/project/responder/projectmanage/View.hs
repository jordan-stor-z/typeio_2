{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.ProjectManage.View where

import Lucid
import Common.Web.Template.MainHeader (mainHeaderTemplate)
import Control.Monad        (guard)
import Control.Monad.Reader (ReaderT)
import Data.Aeson           (encode, ToJSON(..), (.=), object)
import Data.Int             (Int64)
import Data.Text            (pack, Text, unpack)
import Data.Text.Encoding   (decodeUtf8)
import Data.Time.Clock      (UTCTime)
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
import Database.Persist     (Entity(..))
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Network.HTTP.Types   (status200, status403)
import Network.Wai          (Application, responseLBS)
import Text.Read            (readMaybe)
import qualified Domain.Project.Model as M ( Dependency(..)
                                           , Node(..)
                                           , Project(..)
                                           , unNodeStatusKey
                                           , unNodeTypeKey
                                           )

data Dependency = Dependency
  { dependencyId :: Int64
  , childNodeId  :: Int64
  , parentNodeId :: Int64
  }

data Node = Node
  { nodeId          :: Int64
  , nodeName        :: String
  , nodeDescription :: String
  , nodeStatusId    :: String
  , nodeTitle       :: String
  , nodeTypeId      :: String
  , nodeUpdated     :: UTCTime 
  }

data GraphNode = GraphNode 
  { graphNodeId :: Int64
  , label :: Text
  , pinned :: Bool
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
  toJSON (GraphNode gid lbl pnd) =
    object [ "id" .= gid
           , "label" .= lbl
           , "pinned" .= pnd
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

type ChildNode  = Node
type ParentNode = Node

type ProjectId = Text

buildGraph' :: [Node] -> [Dependency] -> Graph 
buildGraph' ns ds = 
  let ls  = map (\d -> GraphLink (childNodeId d) (parentNodeId d)) ds
      ns' = do
        n <- ns
        let pnd = nodeTypeId n == "project_root"
        return $ GraphNode 
          { graphNodeId = nodeId n
          , label = pack $ nodeName n
          , pinned = pnd
          }
  in Graph ns' ls 

buildGraph :: [Node] -> [Dependency] -> [(ParentNode, [ChildNode])] 
buildGraph ns ds = do
        n <- ns
        let cs = do
              d <- ds
              c <- ns
              guard $ parentNodeId d == nodeId n
              guard $ childNodeId d == nodeId c
              return c 
        return (n, cs) 

handleProjectManageView :: ConnectionPool -> ProjectId -> Application
handleProjectManageView pl p _ respond = do
  let pM = readMaybe (unpack p) :: Maybe Int64 
  case pM of
    Nothing  -> respondBadProjectId
    Just pid -> do
      (ns, ds) <- flip runSqlPool pl $ do 
        n <- queryNodes pid
        d <- queryDependencies $ map nodeId n
        return (n, d)
      let grp = buildGraph' ns ds
      respond $ responseLBS
        status200 
        [("Content-Type", "text/html")]
        (renderBS $ templateTree grp)
  where
    respondBadProjectId = respond $ responseLBS
      status403
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= ("Invalid project ID" :: Text)])

toDependencySchema :: Entity M.Dependency -> Dependency
toDependencySchema (Entity k d) = 
  Dependency 
    { dependencyId = fromSqlKey k
    , childNodeId  = fromSqlKey . M.dependencyNodeId $ d 
    , parentNodeId = fromSqlKey . M.dependencyToNodeId $ d 
    }

toNodeSchema :: Entity M.Node -> Node
toNodeSchema (Entity k e) = Node 
  { nodeId          = fromSqlKey k
  , nodeName        = M.nodeTitle e
  , nodeDescription = M.nodeDescription e
  , nodeStatusId    = M.unNodeStatusKey . M.nodeNodeStatusId $ e
  , nodeTitle       = M.nodeTitle e
  , nodeTypeId      = M.unNodeTypeKey . M.nodeNodeTypeId $ e
  , nodeUpdated     = M.nodeUpdated e
  }

queryNodes :: Int64 -> ReaderT SqlBackend IO [Node] 
queryNodes pid = do
  let pkey = toSqlKey @M.Project pid 
  ns <- select $ do
    n <- from $ table @M.Node
    where_ (n.projectId ==. val pkey)
    pure n
  return $ toNodeSchema <$> ns

queryDependencies :: [Int64] -> ReaderT SqlBackend IO [Dependency]
queryDependencies nids = do
  let nkeys = toSqlKey @M.Node <$> nids
  ds <- select $ do
    d <- from $ table @M.Dependency
    where_ (d.nodeId `in_` valList nkeys)
    pure d
  return $ toDependencySchema <$> ds

templateTree :: Graph -> Html ()
templateTree g = do
  mainHeaderTemplate "Project"
  let empty = mempty :: Html ()
  let js = encode g
  link_ [ rel_ "stylesheet"
        , href_ "/static/styles/views/manage-project.css"
        ]
  script_ [src_ "https://unpkg.com/d3@7"]              empty 
  script_ [id_ "graph-data", type_ "application/json"] js 
  script_ [src_ "/static/script/nodetree.js"]          empty 
  svg_     [ id_     "tree-view"
           , height_ "800"
           , width_  "100%"
           ] empty
