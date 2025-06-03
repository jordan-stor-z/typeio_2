{-# LANGUAGE OverloadedStrings #-}

module Config.Web where

import Config.Load (ConfigError, getVal)
import Control.Monad (when)
import Control.Monad.Writer (WriterT, tell)
import Data.Aeson ((.=), ToJSON, toJSON, object)
import Data.ByteString.Char8 (pack, unpack)
import Data.CaseInsensitive (mk, original)
import Data.Maybe (isNothing)
import Network.HTTP.Types (HeaderName)
import Text.Read (readMaybe)

webPort :: String
webPort = "WEB_PORT"

webRedirectPath :: String
webRedirectPath = "WEB_REDIRECT_PATH"

webRequestIdHeader :: String
webRequestIdHeader = "WEB_REQUEST_ID_HEADER"

data WebConfig = WebConfig 
  { port            :: Int 
  , redirectPath    :: String
  , requestIdHeader :: HeaderName 
  }
  deriving (Read, Show, Eq)

instance ToJSON WebConfig where
  toJSON cfg =
    object [ "port" .= port cfg
            , "requestIdHeader" .= (unpack . original $ requestIdHeader cfg) 
           ]

loadWebConfig :: WriterT [ConfigError] IO (Maybe WebConfig)
loadWebConfig = do
  prt <- getVal webPort >>= readPort
  rdp <- getVal webRedirectPath
  wri <- getVal webRequestIdHeader
  return $ WebConfig <$> prt <*> rdp <*> fmap (mk . pack) wri 

readPort :: Maybe String -> WriterT [ConfigError] IO (Maybe Int)
readPort Nothing = return Nothing
readPort p = do
  let i = p >>= readMaybe
  when (isNothing i) $
    tell ["Invalid port value: " ++ show p]
  return i

