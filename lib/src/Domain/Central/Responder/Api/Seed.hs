{-# LANGUAGE OverloadedStrings #-}

{- | Seeding a development database: the reference data every install
needs, plus one demo project with a dependency graph worth drawing.

The demo project exists because until it did, __no project had a single
dependency__. @Api.Node.Post@ was the only writer of
@project.dependency@ and #198 removed the rows it wrote, so on a fresh
database every graph was a handful of disconnected nodes and no edges
(#243). There is still no way to create a dependency through the app —
that is #205 — so this is currently the only way to see the graph
render as anything at all.
-}
module Domain.Central.Responder.Api.Seed where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import qualified Data.Map.Strict as M
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (insert, insertUnique, insert_, selectFirst, (==.))
import Database.Persist.Postgresql (ConnectionPool)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Domain.Project.Model
  ( Dependency (..)
  , Node (..)
  , NodeStatus (..)
  , NodeStatusId
  , NodeType (..)
  , NodeTypeId
  , ProjectId
  )
import qualified Domain.Project.Model as M
import Network.HTTP.Types (status200)
import Network.Wai (Response, ResponseReceived, responseLBS)

handleSeedDatabase :: ConnectionPool -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleSeedDatabase pool respond = do
  flip runSqlPool pool $ do
    mapM_ insertUnique nodeStatuses
    mapM_ insertUnique nodeTypes
    seedDemoProject
  respond $
    responseLBS
      status200
      [("Content-Type", "application/json")]
      "Database seeded successfully"

nodeTypes :: [NodeType]
nodeTypes =
  [ NodeType "project_root"
  , NodeType "work"
  ]

nodeStatuses :: [NodeStatus]
nodeStatuses =
  [ NodeStatus "active"
  , NodeStatus "closed"
  , NodeStatus "open"
  , NodeStatus "rejected"
  ]

{- | The title of the demo project's root node, and the marker that says
the demo has already been seeded.

Neither @project@ nor @node@ has a natural unique key, so 'insertUnique'
cannot make this idempotent the way it does for the reference data. A
root node with this exact title is the stand-in.
-}
demoRootTitle :: String
demoRootTitle = "Public API launch"

{- | The demo project's work, tagged @A@-@G@ to match the worked example
in @docs\/architecture\/orbital-dependency-weighted-graph.md@ and the
fixture in @Domain.Project.Orbit.UnfoldSpec@.

The tags are only for wiring 'demoDependencies' below; nothing stores
them.
-}
demoWork :: [(Char, String, String)]
demoWork =
  [ ('A', "Publish the launch post", "Announcement, once there is something to announce.")
  , ('B', "Ship the mobile client", "The client everyone is actually waiting for.")
  , ('C', "Stabilise the public API", "Freeze the surface and stop breaking callers.")
  , ('D', "Run the beta programme", "A dozen friendly teams, in production.")
  , ('E', "Finish the auth service", "Tokens, refresh, revocation.")
  , ('F', "Open the partner sandbox", "Somewhere partners can integrate safely.")
  , ('G', "Write the integration guide", "The document a partner reads first.")
  ]

{- | @(dependent, dependency)@: the first is waiting on the second.

Deliberately the shape from the architecture doc's worked example, which
is what makes this project worth looking at in every visualization:

* __Three heads__ — @A@, @B@ and @F@ — so the drawing has more than one
  work stream and, in the orbital visualization, a meaningful empty eye.
* __A shared bottleneck.__ @E@ (the auth service) is waited on by both
  @D@ and @C@, and @C@ is itself waited on by @B@ and @F@. In the
  orbital drawing @E@ is therefore replicated three times and @C@ twice
  — ten discs for seven nodes — which is the entire premise of that
  visualization and unreachable without a graph shaped like this.
* __A replicated subtree, not just a replicated node.__ @C@ carries @E@
  with it wherever it is drawn.
* __A node with several dependencies.__ @F@ waits on both @G@ and @C@
  and is still drawn once, since replication follows dependents rather
  than dependencies — the distinction most easily got backwards.

__No cycles__, per the decision recorded on #205.
-}
demoDependencies :: [(Char, Char)]
demoDependencies =
  [ ('A', 'D') -- the launch post waits on the beta programme
  , ('D', 'E') -- the beta waits on auth
  , ('B', 'C') -- the mobile client waits on a stable API
  , ('C', 'E') -- the stable API waits on auth
  , ('F', 'G') -- the sandbox waits on the integration guide
  , ('F', 'C') -- and on the stable API
  ]

{- | Insert the demo project, unless it is already there.

Idempotent by the same contract as the reference data above: running
@make seed-db@ twice leaves one demo project, not two.
-}
seedDemoProject :: MonadIO m => ReaderT SqlBackend m ()
seedDemoProject = do
  existing <- selectFirst [M.NodeTitle ==. demoRootTitle] []
  case existing of
    Just _ -> pure ()
    Nothing -> do
      now <- liftIO getCurrentTime
      projectKey <- insert M.Project
      let node = demoNode now projectKey
      _ <-
        insert
          . node rootType demoRootTitle
          $ "Everything the launch is waiting on."
      workKeys <-
        mapM
          (\(_, title, desc) -> insert (node workType title desc))
          demoWork
      let keyOf = M.fromList (zip [t | (t, _, _) <- demoWork] workKeys)
      forM_ demoDependencies $ \(dependent, dependency) ->
        forM_ ((,) <$> M.lookup dependent keyOf <*> M.lookup dependency keyOf) $
          \(a, b) -> insert_ (Dependency a b)
  where
    rootType = M.NodeTypeKey "project_root"
    workType = M.NodeTypeKey "work"

{- | A node on the demo project. Both reference rows it points at are
inserted in the same transaction, just above.
-}
demoNode ::
  UTCTime ->
  ProjectId ->
  NodeTypeId ->
  String ->
  String ->
  Node
demoNode now projectKey typeKey title desc =
  M.Node
    { M.nodeCreated = now
    , M.nodeDeleted = Nothing
    , M.nodeDescription = desc
    , M.nodeNodeStatusId = activeStatus
    , M.nodeNodeTypeId = typeKey
    , M.nodeProjectId = projectKey
    , M.nodeTitle = title
    , M.nodeUpdated = now
    }

activeStatus :: NodeStatusId
activeStatus = M.NodeStatusKey "active"
