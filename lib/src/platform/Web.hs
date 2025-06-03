{-# LANGUAGE OverloadedStrings #-}

module Platform.Web (start) where

import Config.App (AppConfig(..), loadConfig)
import Config.Web (WebConfig(..))
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, try)
import Domain.Admin.Application.Config (admin)
import Data.Text (Text)
import Environment.Env (Env(..), withEnv)
import Network.HTTP.Types (status200, status404, Method)
import Network.Wai (Application, responseLBS, pathInfo, requestMethod)
import Network.Wai.Handler.Warp (run)
import Web.Middleware (allMiddleware)

loadDotEnv :: IO ()
loadDotEnv = do
  _ <- try $ loadFile defaultConfig :: IO (Either SomeException ())
  return ()

start :: IO ()
start = do
  loadDotEnv
  cfg <- loadConfig
  let prt = port . web $ cfg
      app' = allMiddleware <*> app
  withEnv cfg $ run prt . app' 

app :: Env -> Application
app ev req respond =  do
  case appRoutes ev mth pth of
    Just ap -> ap req respond
    Nothing -> notFound req respond 
  where 
    pth = pathInfo req
    mth = requestMethod req

notFound :: Application
notFound _ respond = respond $ responseLBS status404 [] "Not Found" 

appRoutes :: Env -> Method -> [Text] -> Maybe Application
appRoutes _ _ [] = Just $ \_ respond -> respond $ responseLBS status200 [] "Index"
appRoutes ev mt (p:ps) = case p of
    "admin" -> admin ev mt ps
    _         -> Nothing

  
