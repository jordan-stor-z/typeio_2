{-# LANGUAGE OverloadedStrings #-}

module Config.Web where

import Common.Validation    ( (.$) 
                            , isThere
                            , isNotEmpty
                            , isBetween
                            , ValidationErr
                            , valRead
                            )
import Control.Monad.Writer (Writer)
import Data.Aeson           ((.=), ToJSON, toJSON, object)
import Data.CaseInsensitive (mk, original)
import Data.Text            (pack)
import Network.HTTP.Types   (HeaderName)
import System.Environment   (lookupEnv)
import qualified Data.ByteString.Char8 as B (pack, unpack)

webIndexRedirect :: String
webIndexRedirect = "WEB_INDEX_REDIRECT"

webPort :: String
webPort = "WEB_PORT"

webRequestIdHeader :: String
webRequestIdHeader = "WEB_REQUEST_ID_HEADER"

data LookupWebConfig = LookupWebConfig 
  { loadIndexRedirect   :: Maybe String 
  , loadPort            :: Maybe String 
  , loadRequestIdHeader :: Maybe String 
  }

data WebConfig = WebConfig 
  { indexRedirect   :: String 
  , port            :: Int 
  , requestIdHeader :: HeaderName 
  }
  deriving (Read, Show, Eq)

instance ToJSON WebConfig where
  toJSON cfg =
    object [ "port"             .= port cfg
            , "requestIdHeader" .= (B.unpack . original $ requestIdHeader cfg) 
           ]

loadWebConfig :: IO (Writer [ValidationErr] (Maybe WebConfig))
loadWebConfig = validateConfig <$> lookupWebConfig

lookupWebConfig :: IO LookupWebConfig
lookupWebConfig = do
  redir <- lookupEnv webIndexRedirect
  port' <- lookupEnv webPort
  print "PORT ::::"
  print port'
  reqid <- lookupEnv webRequestIdHeader
  return $ LookupWebConfig 
    { loadIndexRedirect   = redir
    , loadPort            = port'
    , loadRequestIdHeader = reqid
    }

validateConfig :: LookupWebConfig -> Writer [ValidationErr] (Maybe WebConfig)
validateConfig c = do
  redir <- loadIndexRedirect c
           .$ id
           >>= isThere (er webIndexRedirect)
  port' <- loadPort c
            .$ id
            >>= isThere           (er webPort)
            >>= isNotEmpty        (er webPort)
            >>= valRead           "WEB_PORT must be a valid integer"
            >>= isBetween 1 65535 (er webPort)
  reqid <- loadRequestIdHeader c
            .$ id
            >>= isThere (er webRequestIdHeader)
            >>= isNotEmpty (er webRequestIdHeader)
  return $ WebConfig <$> redir <*> port'  <*> (mk . B.pack <$> reqid)
  where
    er k = pack k <> " is missing from environment config"

