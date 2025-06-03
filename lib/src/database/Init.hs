module Database.Init where

import Config.Db (DbConfig, connStr)
import Control.Exception (SomeException, try)
import Data.ByteString.Char8 (pack)
import Data.Pool (destroyAllResources)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Logging (runDatabaseLoggingT)

data DatabaseInit = DatabaseInit
  { cleanup :: IO ()
  , pool :: ConnectionPool
  }

initPool :: DbConfig -> IO (Either SomeException DatabaseInit)
initPool cf = do
  result <- try $ runDatabaseLoggingT $ createPostgresqlPool cs 10 
    :: IO (Either SomeException ConnectionPool)
  let clp = destroyAllResources <$> result
  return $ DatabaseInit <$> clp <*> result 
  where
    cs = pack . connStr $ cf

