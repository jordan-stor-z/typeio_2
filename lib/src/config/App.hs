{-# LANGUAGE OverloadedStrings #-}

module Config.App where

import Config.Db (DbConfig, loadDbConfig)
import Config.Load (ConfigError, getVal) 
import Config.Web (WebConfig(..), loadWebConfig)
import Control.Monad.Writer (runWriterT, WriterT, tell)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Aeson ((.=), ToJSON, toJSON, object)
import Text.Read (readMaybe)

keyEnv :: String
keyEnv = "ENV"

data AppConfig = AppConfig
  { envName :: EnvironmentName
  , dbConf  :: DbConfig
  , webConf :: WebConfig
  } deriving (Read, Show)

instance ToJSON AppConfig where
  toJSON cfg =
    object [ "env" .= envName cfg
           , "db"  .= dbConf  cfg
           , "web" .= webConf cfg
           ]

data EnvironmentName = Local | Development | Production
  deriving (Eq, Read, Show)

instance ToJSON EnvironmentName where
  toJSON = toJSON . show

loadAppConfig :: WriterT [ConfigError] IO (Maybe AppConfig) 
loadAppConfig = do
  ev <- getVal keyEnv >>= readEnv
  db <- loadDbConfig
  wb <- loadWebConfig
  return $ AppConfig <$> ev <*> db <*> wb

loadConfig :: IO AppConfig
loadConfig = do 
  (config, errors) <- runWriterT loadAppConfig
  case config of
    Nothing  -> error $ "Failed to load configuration: " ++ unlines errors
    Just cfg -> return cfg

readEnv :: Maybe String -> WriterT [ConfigError] IO (Maybe EnvironmentName)
readEnv Nothing = return Nothing
readEnv em = do
  let ev = em >>= readMaybe
  when (isNothing ev) $
    tell ["Invalid environment value: " ++ show em] 
  return ev

webDefaultPath :: AppConfig -> String
webDefaultPath = indexRedirect . webConf
