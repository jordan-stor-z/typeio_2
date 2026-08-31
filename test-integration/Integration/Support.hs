{-# LANGUAGE OverloadedStrings #-}

-- | Shared infrastructure for the @integration@ test-suite: starting a
-- disposable Postgres container, migrating and seeding it, and tearing
-- it down again. One container is meant to be started per suite run
-- (via Hspec's 'Test.Hspec.aroundAll', not 'Test.Hspec.around') and
-- reused across every test in that run, with 'resetBetweenTests'
-- truncating mutable data between individual tests.
--
-- See @docs/solution-proposals/integration-testing.md@ (§4-§6) for the
-- reasoning behind each choice here.
module Integration.Support
  ( withTestDatabase
  , resetBetweenTests
  , seedProjectWithRootNode
  ) where

import Config.Db                        (DbConfig(..))
import Control.Exception                (ErrorCall(..), throwIO)
import Control.Monad.Cont               (runContT)
import Control.Monad.IO.Class           (liftIO)
import Data.Text                        (Text)
import qualified Data.Text as T
import Data.Time                        (getCurrentTime)
import Data.Word                        (Word16)
import Database.Persist                 ((==.), Entity(..), Key, insert, insertUnique, selectList)
import Database.Persist.Sql             (ConnectionPool, rawExecute, runSqlPool)
import qualified Domain.Central.Responder.Api.Seed as Seed
import qualified Domain.Project.Model   as M
import Environment.Db                   (withPool)
import System.Exit                      (ExitCode(..))
import System.Process                   (readProcessWithExitCode)
import qualified TestcontainersPostgresql as TCPG

-- | Credentials for the disposable container. The official Postgres
-- image creates a database named after @POSTGRES_USER@ when
-- @POSTGRES_DB@ isn't set, so this value doubles as the database name
-- too -- see 'testDbConfig'.
testDbUser :: Text
testDbUser = "typeio_test"

testDbPassword :: Text
testDbPassword = "typeio_test"

-- | Starts a @postgres:15@ container (same version
-- @local/script/start-postgres.sh@ uses), applies @migrations/@ against
-- it with the same @migrate@ CLI the rest of the project uses, seeds
-- the required @NodeStatus@\/@NodeType@ reference data (the same lists
-- @Domain.Central.Responder.Api.Seed@ inserts on app startup -- reused
-- directly, not duplicated), and hands the action a ready
-- 'ConnectionPool'. The container is torn down once the action
-- returns.
withTestDatabase :: (ConnectionPool -> IO ()) -> IO ()
withTestDatabase action =
  TCPG.run pgConfig $ \(dbHost, dbPort') -> do
    runMigrations dbHost dbPort'
    runContT (withPool $ testDbConfig dbHost dbPort') $ \pool -> do
      seedReferenceData pool
      action pool
  where
    pgConfig = TCPG.Config
      { TCPG.tagName     = "postgres:15"
      , TCPG.auth        = TCPG.CredentialsAuth testDbUser testDbPassword
      , TCPG.forwardLogs = False
      }

testDbConfig :: Text -> Word16 -> DbConfig
testDbConfig dbHost dbPort' = DbConfig
  { database  = T.unpack testDbUser
  , host      = T.unpack dbHost
  , password  = T.unpack testDbPassword
  , dbPort    = show dbPort'
  , poolCount = 5
  , schema    = "project"
  , user      = T.unpack testDbUser
  }

runMigrations :: Text -> Word16 -> IO ()
runMigrations dbHost dbPort' = do
  (code, out, err) <- readProcessWithExitCode "migrate"
    [ "-path", "migrations"
    , "-database", migrationUrl dbHost dbPort'
    , "up"
    ]
    ""
  case code of
    ExitSuccess   -> pure ()
    ExitFailure _ -> throwIO . ErrorCall $
      "migrate failed against the integration-test container:\n" <> out <> err

-- | Same @postgres://...?sslmode=disable@ shape as the Makefile's
-- @DB_URL@, pointed at the container's mapped host/port instead of the
-- dev database.
migrationUrl :: Text -> Word16 -> String
migrationUrl dbHost dbPort' = T.unpack $
  "postgres://" <> testDbUser <> ":" <> testDbPassword
    <> "@" <> dbHost <> ":" <> T.pack (show dbPort')
    <> "/" <> testDbUser <> "?sslmode=disable"

seedReferenceData :: ConnectionPool -> IO ()
seedReferenceData pool = flip runSqlPool pool $ do
  mapM_ insertUnique Seed.nodeStatuses
  mapM_ insertUnique Seed.nodeTypes

-- | Clears every table a test might have written to, without touching
-- the reference data 'seedReferenceData' inserted once at container
-- startup. Truncating all three together (rather than deleting in
-- dependency order) sidesteps FK ordering entirely.
truncateTestData :: ConnectionPool -> IO ()
truncateTestData pool = flip runSqlPool pool $
  rawExecute
    "TRUNCATE project.dependency, project.node, project.project RESTART IDENTITY CASCADE"
    []

-- | Hspec @beforeWith@-shaped hook: truncate mutable tables before each
-- test, then hand the same pool through unchanged. Truncating instead
-- of wrapping each test in a rolled-back transaction is deliberate --
-- responders commit their own transaction via 'runSqlPool', so there's
-- no outer transaction for a test to roll back (see the proposal's §5).
resetBetweenTests :: ConnectionPool -> IO ConnectionPool
resetBetweenTests pool = truncateTestData pool >> pure pool

-- | Minimal fixture every write-responder test needs: a bare 'M.Project'
-- and a root 'M.Node' attached to it (status @active@, type
-- @project_root@ -- both from 'seedReferenceData'). Centralized here
-- since every mutating-responder integration test (this pilot and its
-- follow-ups, #66-#69) needs the same starting point.
seedProjectWithRootNode :: ConnectionPool -> IO (Key M.Project, Key M.Node)
seedProjectWithRootNode pool = flip runSqlPool pool $ do
  now <- liftIO getCurrentTime
  projectKey   <- insert M.Project
  activeStatus <- selectList [M.NodeStatusNodeStatusId ==. "active"] []
  rootType     <- selectList [M.NodeTypeNodeTypeId ==. "project_root"] []
  statusKey <- keyOrErr "NodeStatus \"active\"" activeStatus
  typeKey   <- keyOrErr "NodeType \"project_root\"" rootType
  rootKey <- insert M.Node
    { M.nodeCreated      = now
    , M.nodeDeleted      = Nothing
    , M.nodeDescription  = "Root node"
    , M.nodeNodeStatusId = statusKey
    , M.nodeNodeTypeId   = typeKey
    , M.nodeProjectId    = projectKey
    , M.nodeTitle        = "Root"
    , M.nodeUpdated      = now
    }
  pure (projectKey, rootKey)
  where
    keyOrErr label rows = case rows of
      (e : _) -> pure (entityKey e)
      []      -> liftIO . throwIO . ErrorCall $
        "seedProjectWithRootNode: expected seeded " <> label <> " but found none"
