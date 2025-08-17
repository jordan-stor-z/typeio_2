{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.Ui.ProjectManage.Graph where

import Common.Web.Attributes
import Common.Web.Elements
import Domain.Project.Responder.Ui.ProjectManage.Link
import Lucid 
import Common.Validation          ( (.$)
                                  , isNotEmpty
                                  , isThere
                                  , runValidation
                                  , ValidationErr
                                  , valRead
                                  )
import Common.Web.Query           (lookupVal)
import Control.Monad              (forM_)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Either (hoistEither, runEitherT)
import Control.Monad.Reader       (ReaderT)
import Data.Aeson                 (encode, ToJSON(..), (.=), object)
import Data.Either                (notNullEither)
import Data.Int                   (Int64)
import Data.Bifunctor             (first)
import Data.Text                  (pack, Text, unpack)
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
import Database.Persist                (Entity(..))
import Database.Persist.Sql            (ConnectionPool, SqlBackend, runSqlPool)
import Network.HTTP.Types              (status200, status403, queryToQueryText)
import Network.HTTP.Types.URI          (QueryText)
import Network.Wai                     ( Application
                                       , responseLBS
                                       , Request (queryString)
                                       )
import qualified Domain.Project.Model as M ( Dependency(..)
                                           , Node(..)
                                           , Project(..)
                                           , unNodeTypeKey
                                           )

data GetGraphError = 
  InvalidParams [ValidationErr]
  | MissingNodes 

data Graph = Graph
  { links :: [GraphLink]
  , nodes :: [GraphNode]
  }

data GraphNode = GraphNode 
  { graphNodeId :: Int64
  , label       :: Text
  , nodeType    :: Text
  , projectId   :: Int64
  }

data GraphLink = GraphLink
  { source :: Int64
  , target :: Int64
  }

instance ToJSON Graph where
  toJSON (Graph ns ls) =
    object [ "links" .= ls
           , "nodes" .= ns
           ]

instance ToJSON GraphLink where
  toJSON (GraphLink src tgt) =
    object [ "source" .= src
           , "target" .= tgt
           ]

instance ToJSON GraphNode where
  toJSON (GraphNode gid pid typ lbl) =
    object [ "id"        .= gid
           , "projectId" .= pid
           , "label"     .= lbl
           , "nodeType"  .= typ 
           ]

classNodeType :: GraphNode -> Text
classNodeType n = if nodeType n == "project_root" 
                  then "root" 
                  else "work"

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
           . fmap (fromSqlKey . entityKey) 
           $ ns
    pure . toGraph ns . fmap entityVal $ ds 
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

pushUrl:: Int64 -> Int64 -> Text
pushUrl nid pid = "/ui/project/vw"
                   <> "?projectId=" 
                   <> (pack . show $ pid)
                   <> "&nodeId=" 
                   <> (pack . show $ nid)

queryNodes :: Int64 -> ReaderT SqlBackend IO [Entity M.Node]
queryNodes pid = do
  select $ do
    n <- from $ table @M.Node
    where_ (n.projectId ==. val pkey)
    pure n
  where 
    pkey = toSqlKey @M.Project pid 

queryDependencies :: [Int64] -> ReaderT SqlBackend IO [Entity M.Dependency]
queryDependencies []   = return []
queryDependencies nids = do
  select $ do
    d <- from $ table @M.Dependency
    where_ (d.nodeId `in_` valList nkeys)
    pure d
  where
    nkeys = toSqlKey @M.Node <$> nids

templateGraph :: Graph -> Html ()
templateGraph g = do
  script_ [id_ "graph-data", type_ "application/json"] $ encode g 
  script_ [src_ "/static/script/nodetree2.js"] empty 
  svg_    [ id_     "tree-view"
          , height_ "100%"
          , width_  "100%"
          , h_ "on load transition my opacity to 1 over 200ms"
          ] $ do 
    defs_ [ id_           "arrow" 
          , viewBox_      "0 -5 10 10"
          , refX_         "30"
          , refY_         "0"
          , markerWidth_  "6"
          , markerHeight_ "6"
          , orient_       "auto"
          ] $ do 
      path_ [ d_    "M0,-5L10,0L0,5"
            , fill_ "#999"
            ] empty
    g_ [class_ "zoom-group"] $ do 
      g_ [class_ "links"] $ do
        forM_ (links g) $ \_ -> 
          line_ [ class_         "link"
                , stroke_        "#999"
                , strokeOpacity_ "0.6"
                , strokeWidth_   "2"
                , markerEnd_     "url(#arrow)"] empty
      g_ [class_ "nodes"] $ do
        forM_ (nodes g) $ \n -> do 
          g_ [ id_ $       "node-" <> (pack . show . graphNodeId $ n)
             , class_      "node"
             , hxGet_      $ nodePanelLink
                              (graphNodeId n) 
                              (projectId n)
             , hxTrigger_  "click"
             , hxTarget_   "#node-panel"
             , hxPushUrl'_ $ pushUrl 
                              (graphNodeId n) 
                              (projectId n) 
             , hxSwap_     "innerHTML"
             ] $ do
            circle_ [ class_ $ classNodeType n 
                    , stroke_      "white"
                    , strokeWidth_ "1.5"
                    ] empty
            text_ [ id_       $ "node-text-" 
                                <> (pack . show . graphNodeId $ n)
                  , fontSize_   "10"
                  , textAnchor_ "middle"
                  , dy_         "0.35em"
                  , fill_       "white"
                  ] (toHtml . label $ n)
      g_ [ id_ "updaters" ] $ do
        forM_ (nodes g) $ \n -> do 
            g_ [ class_ "hidden"
               , h_ $ "on nodePanel:onEditClosed(nodeId)[nodeId=="
                    <> (pack . show . graphNodeId $ n)
                    <> "] from #node-panel trigger click on me"
               , hxGet_ $ nodeRefreshLink 
                          (graphNodeId n) 
                          (projectId n) 
                          (label n) 
               , hxTrigger_   "click"
               , hxTarget_ $  "#node-text-" 
                              <> (pack . show . graphNodeId $ n)
               , hxSwap_      "innerHTML"
               , hxPushUrl_   False
                ] empty
  where
    empty = mempty :: Html ()

toGraph :: [Entity M.Node] -> [M.Dependency] -> Graph 
toGraph ns ds = Graph (map toLink ds) (map toGNode ns)
  where
    toLink d = GraphLink 
          { source = fromSqlKey . M.dependencyNodeId $ d
          , target = fromSqlKey . M.dependencyToNodeId $ d
          }
    toGNode (Entity k e) = GraphNode 
          { graphNodeId = fromSqlKey k 
          , projectId   = fromSqlKey . M.nodeProjectId $ e
          , label       = pack . M.nodeTitle $ e
          , nodeType    = pack . M.unNodeTypeKey . M.nodeNodeTypeId $ e
          }

validateProjectId ::  QueryText -> Either [ValidationErr] Int64
validateProjectId qt = runValidation id $ do
  lookupVal "projectId" qt 
    .$ unpack
    >>= isThere    "Project id must be present" 
    >>= isNotEmpty "Project id must have a value"
    >>= valRead    "Project id must be valid integer" 
