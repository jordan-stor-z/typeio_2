{-# LANGUAGE FlexibleContexts #-}
module Environment.Db where

import Config.Db (DbConfig(..), connStr)
import Control.Monad.Cont (ContT(..)) 
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Logging (runDatabaseLoggingT)

withPool :: DbConfig -> ContT r IO (Pool SqlBackend)
withPool cf = ContT with'
  where
    cc = poolCount cf
    cs = pack . connStr $ cf
    with' k = runDatabaseLoggingT $ withPostgresqlPool cs cc $ liftIO . k

