module Config.Load (ConfigError, getVal) where

import Control.Monad.Writer (lift, WriterT, tell)
import System.Environment (lookupEnv)

type ConfigError = String

getVal :: String -> WriterT [ConfigError] IO (Maybe String)
getVal key = do
  val <- lift $ lookupEnv key
  case val of
    Nothing -> tell ["Missing key: " ++ key] >> return Nothing
    Just v  -> return (Just v)


