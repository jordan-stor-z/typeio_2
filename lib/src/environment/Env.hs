module Environment.Env where

import Config.App (AppConfig(..))
import Control.Monad.Cont (ContT(..), runContT)
import Database.Persist.Sql (ConnectionPool)
import Environment.Db (withPool)
import Environment.Logging (withLogger)
import Logging.Core (EntryLog)

data Env = Env
  { config  :: AppConfig
  , logger  :: EntryLog 
  , pool    :: ConnectionPool
  }

type LoadEnvError = String

withEnv :: AppConfig -> (Env -> IO r) -> IO r
withEnv cf = runContT (Env cf <$> withLogger <*> withPool (db cf))


