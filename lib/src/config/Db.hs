{-# LANGUAGE OverloadedStrings #-}

module Config.Db where

import Data.Aeson ((.=), ToJSON, toJSON, object)
import Config.Load (ConfigError, getVal)
import Control.Monad (when)
import Control.Monad.Writer (tell, WriterT)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)

keyDb :: String
keyDb = "DB_DATABASE"

keyHost :: String
keyHost = "DB_HOST"

keyPass :: String
keyPass = "DB_PASS"

keyPort :: String
keyPort = "DB_PORT"

keyPoolCount :: String
keyPoolCount = "DB_POOL_COUNT"

keySchema :: String
keySchema = "DB_SCHEMA"

keyUser :: String
keyUser = "DB_USER"

data DbConfig = DbConfig 
  { database  :: String
  , host      :: String 
  , password  :: String
  , dbPort    :: String
  , poolCount :: Int
  , schema    :: String
  , user      :: String
  } deriving (Read, Show)

instance ToJSON DbConfig where
  toJSON cfg =
    object [ "database"  .= database cfg
           , "host"      .= host cfg
           , "password"  .= password cfg 
           , "port"      .= dbPort cfg
           , "poolCount" .= poolCount cfg
           , "user"      .= user cfg
           ]

connStr :: DbConfig -> String
connStr cfg =
  "host="         <> host cfg     <>
  " dbname="      <> database cfg <>
  " user="        <> user cfg     <>
  " password="    <> password cfg <>
  " port="        <> dbPort cfg   <>
  " sslmode=disable"

loadDbConfig :: WriterT [ConfigError] IO (Maybe DbConfig)
loadDbConfig = do
  nme <- getVal keyDb 
  hst <- getVal keyHost
  pss <- getVal keyPass 
  prt <- getVal keyPort
  scm <- getVal keySchema
  usr <- getVal keyUser
  plc <- getVal keyPoolCount >>= readPoolCount
  return $ DbConfig 
    <$> nme 
    <*> hst 
    <*> pss 
    <*> prt 
    <*> plc 
    <*> scm
    <*> usr 

readPoolCount :: Maybe String -> WriterT [ConfigError] IO (Maybe Int)
readPoolCount Nothing = return Nothing
readPoolCount p = do
  let i = p >>= readMaybe
  when (isNothing i) $
    tell ["Invalid pool count value: " ++ show p]
  return i

