{-# LANGUAGE OverloadedStrings #-}

module Config.App where

import Config.Db (DbConfig, loadDbConfig)
import Config.Load (ConfigError, getVal) 
import Config.Web (WebConfig, loadWebConfig)
import Control.Monad.Writer (runWriterT, WriterT, tell)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Aeson ((.=), ToJSON, toJSON, object)
import Text.Read (readMaybe)

keyEnv :: String
keyEnv = "ENV"

data AppConfig = AppConfig
  { env  :: Environment
  , db   :: DbConfig
  , web  :: WebConfig
  } deriving (Read, Show)

instance ToJSON AppConfig where
  toJSON cfg =
    object [ "env" .= env cfg
           , "db"  .= db cfg
           , "web" .= web cfg
           ]

data Environment = Local | Development | Production
  deriving (Eq, Read, Show)

instance ToJSON Environment where
  toJSON = toJSON . show

loadAppConfig :: WriterT [ConfigError] IO (Maybe AppConfig) 
loadAppConfig = do
  evc <- getVal keyEnv >>= readEnv
  dbc <- loadDbConfig
  wbc <- loadWebConfig
  return $ AppConfig <$> evc <*> dbc <*> wbc

loadConfig :: IO AppConfig
loadConfig = do 
  (config, errors) <- runWriterT loadAppConfig
  case config of
    Nothing  -> error $ "Failed to load configuration: " ++ unlines errors
    Just cfg -> return cfg

readEnv :: Maybe String -> WriterT [ConfigError] IO (Maybe Environment)
readEnv Nothing = return Nothing
readEnv em = do
  let ev = em >>= readMaybe
  when (isNothing ev) $
    tell ["Invalid environment value: " ++ show em] 
  return ev
