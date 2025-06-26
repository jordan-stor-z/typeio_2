{-# LANGUAGE OverloadedStrings #-}

module Config.Db where

import Common.Validation    ( (.$) 
                            , isThere
                            , isNotEmpty
                            , isBetween
                            , ValidationErr
                            , valRead
                            )
import Control.Monad.Writer (Writer)
import Data.Aeson           ((.=), ToJSON, toJSON, object)
import Data.Text            (pack)
import System.Environment   (lookupEnv)

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

data LookupDbConfig = LookupDbConfig 
  { loadDatabase :: Maybe String
  , loadHost     :: Maybe String
  , loadPassword :: Maybe String
  , loadPort     :: Maybe String
  , loadPoolCount :: Maybe String 
  , loadSchema   :: Maybe String
  , loadUser     :: Maybe String
  }

loadDbConfig :: IO (Writer [ValidationErr] (Maybe DbConfig))
loadDbConfig = validateConfig <$> lookupDbConfig

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

lookupDbConfig :: IO LookupDbConfig 
lookupDbConfig = do
  name  <- lookupEnv keyDb
  host' <- lookupEnv keyHost
  pass  <- lookupEnv keyPass
  port  <- lookupEnv keyPort
  plc   <- lookupEnv keyPoolCount
  schma <- lookupEnv keySchema
  user' <- lookupEnv keyUser
  return $ LookupDbConfig name host' pass port plc schma user' 

validateConfig :: LookupDbConfig -> Writer [ValidationErr] (Maybe DbConfig)
validateConfig c = do
  name  <- loadDatabase c
          .$  id
          >>= isThere     (er keyDb) 
          >>= isNotEmpty  (er keyDb)
  host' <- loadHost c
           .$  id
           >>= isThere    (er keyHost)
           >>= isNotEmpty (er keyHost)
  pass  <- loadPassword c
           .$  id
           >>= isThere    (er keyPass)
           >>= isNotEmpty (er keyPass)
  port  <- loadPort c
           .$  id
           >>= isThere    (er keyPort)
           >>= isNotEmpty (er keyPort)
  plct  <- loadPoolCount c
           .$  id
           >>= isThere        (er keyPoolCount)
           >>= isNotEmpty     (er keyPoolCount)
           >>= valRead        (pack keyPoolCount <> " must be a valid integer")
           >>= isBetween 1 11 (pack keyPoolCount <> " must be between 1 and 10")
  schma <- loadSchema c
           .$  id
           >>= isThere     (er keySchema)
           >>= isNotEmpty  (er keySchema)
  user' <- loadUser c
            .$  id
            >>= isThere    (er keyUser)
            >>= isNotEmpty (er keyUser)
  return $ DbConfig <$> name <*> host' <*> pass 
                    <*> port <*> plct <*> schma 
                    <*> user'
  where
    er k = pack k <> " is missing from environment config"
  
