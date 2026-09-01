{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Project.Responder.Ui.ProjectManage.Graph where

import Common.Validation
  ( ValidationErr
  , isNotEmpty
  , isThere
  , runValidation
  , valRead
  , (.$)
  )
import Common.Web.Attributes
import Common.Web.Elements
import Common.Web.Query (lookupVal)
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (hoistEither, runEitherT)
import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.Bifunctor (first)
import Data.Either (notNullEither)
import Data.Int (Int64)
import Data.Text (Text, pack, unpack)
import Data.Text.Util (intToText, wrapLabel)
import Database.Esqueleto.Experimental
  ( from
  , fromSqlKey
  , in_
  , select
  , table
  , toSqlKey
  , val
  , valList
  , where_
  , (==.)
  )
import Database.Persist (Entity (..))
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import qualified Domain.Project.Model as M
  ( Dependency (..)
  , Node (..)
  , Project (..)
  , unNodeTypeKey
  )
import Domain.Project.Responder.Ui.ProjectManage.Link
import Lucid
import Network.HTTP.Types (queryToQueryText, status200, status403)
import Network.HTTP.Types.URI (QueryText)
import Network.Wai
  ( Application
  , Request (queryString)
  , responseLBS
  )

data GetGraphError
  = InvalidParams [ValidationErr]
  | MissingNodes

data Graph = Graph
  { links :: [GraphLink]
  , nodes :: [GraphNode]
  }

data GraphNode = GraphNode
  { graphNodeId :: Int64
  , label :: Text
  , nodeType :: Text
  , projectId :: Int64
  }

data GraphLink = GraphLink
  { source :: Int64
  , target :: Int64
  }

instance ToJSON Graph where
  toJSON g =
    object
      [ "links" .= links g
      , "nodes" .= nodes g
      ]

instance ToJSON GraphLink where
  toJSON (GraphLink src tgt) =
    object
      [ "source" .= src
      , "target" .= tgt
      ]

instance ToJSON GraphNode where
  toJSON nd =
    object
      [ "id" .= graphNodeId nd
      , "projectId" .= projectId nd
      , "label" .= label nd
      , "nodeType" .= nodeType nd
      ]

classNodeType :: GraphNode -> Text
classNodeType n =
  if nodeType n == "project_root"
    then "root"
    else "work"

handleProjectGraph :: ConnectionPool -> Application
handleProjectGraph pl req respond = do
  rslt <- flip runSqlPool pl . runEitherT $ do
    pid <-
      hoistEither
        . first InvalidParams
        . validateProjectId
        $ qt
    ns <-
      lift (queryNodes pid)
        >>= hoistEither
          . notNullEither MissingNodes
    ds <-
      lift
        . queryDependencies
        . fmap (fromSqlKey . entityKey)
        $ ns
    pure . toGraph ns . fmap entityVal $ ds
  case rslt of
    Left (InvalidParams es) -> respondValErrs es
    Left MissingNodes -> respondMissingNodes
    Right graph -> respondSuccess graph
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
    qt =
      queryToQueryText
        . queryString
        $ req

pushUrl :: Int64 -> Int64 -> Text
pushUrl nid pid =
  "/ui/project/vw"
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
queryDependencies [] = return []
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
  svg_
    [ id_ "tree-view"
    , height_ "100%"
    , width_ "100%"
    , h_ "on load transition my opacity to 1 over 200ms"
    ]
    $ do
      -- `marker-end` on the edges resolves `url(#arrow)` and requires
      -- that id to be a `<marker>`. It used to be on the `<defs>`
      -- itself, with the marker's own attributes (viewBox/refX/orient/
      -- markerWidth) hung off `<defs>` where they mean nothing -- so
      -- the reference never resolved to a marker and no arrowhead has
      -- ever actually rendered, leaving the graph showing dependencies
      -- as undirected lines. `<defs>` is just the container; the
      -- `<marker>` inside it is what carries the id and the geometry.
      defs_ []
        $ marker_
          [ id_ "arrow"
          , viewBox_ "0 -5 10 10"
          , -- The arrowhead's own tip (x=10 in the viewBox above) is
            -- what should land on the end of the line; nodetree2.js
            -- already stops each edge at the node's edge rather than
            -- its centre, so the head needs no extra pulling back.
            refX_ "10"
          , refY_ "0"
          , markerWidth_ "6"
          , markerHeight_ "6"
          , orient_ "auto"
          ]
        $ path_
          [ d_ "M0,-5L10,0L0,5"
          , fill_ "#999"
          ]
          empty
      g_ [class_ "zoom-group"] $ do
        g_ [id_ "graph-links"] $ do
          -- A `<path>`, not a `<line>`: nodetree2.js draws each
          -- dependency edge as a gentle curve bowed around the project
          -- root rather than a straight chord, which only a path's `d`
          -- attribute can express. `fill_ "none"` matters here in a way
          -- it never did for `<line>` (which has no fillable area) --
          -- an open curved path without it renders as a solid wedge.
          forM_ (links g) $ \_ ->
            path_
              [ class_ "link"
              , stroke_ "#999"
              , strokeOpacity_ "0.6"
              , strokeWidth_ "2"
              , markerEnd_ "url(#arrow)"
              , fill_ "none"
              ]
              empty
        g_ [id_ "graph-nodes"] $ do
          forM_ (nodes g) $ \n -> do
            g_
              [ id_ $ "node-" <> (intToText . graphNodeId $ n)
              , class_ "node"
              , hxGet_ $
                  nodePanelLink
                    (graphNodeId n)
                    (projectId n)
              , hxTrigger_ "click"
              , hxTarget_ "#node-panel"
              , hxPushUrl'_ $
                  pushUrl
                    (graphNodeId n)
                    (projectId n)
              , hxSwap_ "innerHTML"
              ]
              $ do
                circle_
                  [ class_ $ classNodeType n
                  , stroke_ "white"
                  , strokeWidth_ "1.5"
                  ]
                  empty
                text_
                  [ id_ $
                      "node-text-"
                        <> (intToText . graphNodeId $ n)
                  , fontSize_ "10"
                  , textAnchor_ "middle"
                  , dy_ "0.35em"
                  , fill_ "white"
                  ]
                  $ nodeContents n
  where
    empty = mempty :: Html ()

toGraphNode :: Entity M.Node -> GraphNode
toGraphNode (Entity k e) =
  GraphNode
    { graphNodeId = fromSqlKey k
    , projectId = fromSqlKey . M.nodeProjectId $ e
    , label = pack . M.nodeTitle $ e
    , nodeType = pack . M.unNodeTypeKey . M.nodeNodeTypeId $ e
    }

-- How wide (in characters) and how tall (in lines) a node's label is
-- allowed to get. Sized to sit inside the node circle's own 45px radius
-- at the label font size, so a long title wraps to the node instead of
-- rendering as one runaway line far wider than the node it belongs to
-- -- which is what previously forced the graph's spacing (and with it
-- the whole layout) to sprawl. The untruncated title is always still
-- one click away in the node's detail panel.
labelWidth :: Int
labelWidth = 12

labelLines :: Int
labelLines = 3

nodeContents :: GraphNode -> Html ()
nodeContents n = do
  labelTspans . label $ n
  g_
    [ class_ "hidden"
    , hxGet_ $
        nodeRefreshLink
          (graphNodeId n)
          (projectId n)
          (label n)
    , hxTrigger_ $
        "nodePanel:onEditClosed[event.detail.nodeId=="
          <> (intToText . graphNodeId $ n)
          <> "] from:#node-panel"
    , hxTarget_ $
        "#node-text-"
          <> (intToText . graphNodeId $ n)
    , hxSwap_ "innerHTML"
    , hxPushUrl_ False
    ]
    empty
  where
    empty = mempty :: Html ()

-- SVG `<text>` has no wrapping of its own, so a multi-line label has to
-- be emitted as one `<tspan>` per line. Each line resets `x` to the
-- node's own origin (otherwise tspans just continue along the same
-- line) and steps `dy` by one line height, with the first line lifted
-- by half the block's height so the whole label stays vertically
-- centred on the node however many lines it wraps to.
labelTspans :: Text -> Html ()
labelTspans lbl =
  forM_ (zip [0 :: Int ..] ls) $ \(i, l) ->
    tspan_
      [ x_ "0"
      , dy_ $ if i == 0 then firstDy else lineHeight
      ]
      $ toHtml l
  where
    ls = wrapLabel labelWidth labelLines lbl
    lineHeight = "1.1em"
    firstDy =
      (<> "em")
        . pack
        . show
        $ (-1.1 * fromIntegral (length ls - 1) / 2 :: Double)

toGraph :: [Entity M.Node] -> [M.Dependency] -> Graph
toGraph ns ds = Graph (map toLink ds) (map toGNode ns)
  where
    toLink d =
      GraphLink
        { source = fromSqlKey . M.dependencyNodeId $ d
        , target = fromSqlKey . M.dependencyToNodeId $ d
        }
    toGNode (Entity k e) =
      GraphNode
        { graphNodeId = fromSqlKey k
        , projectId = fromSqlKey . M.nodeProjectId $ e
        , label = pack . M.nodeTitle $ e
        , nodeType = pack . M.unNodeTypeKey . M.nodeNodeTypeId $ e
        }

validateProjectId :: QueryText -> Either [ValidationErr] Int64
validateProjectId qt = runValidation id $ do
  lookupVal "projectId" qt
    .$ unpack
    >>= isThere "Project id must be present"
    >>= isNotEmpty "Project id must have a value"
    >>= valRead "Project id must be valid integer"
