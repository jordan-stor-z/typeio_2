{-# LANGUAGE OverloadedStrings #-}

module Platform.Web (start) where

import Config.App               (AppConfig(..), loadConfig)
import Config.Web               (WebConfig(..))
import Configuration.Dotenv     (defaultConfig, loadFile)
import Container.Build          (withRootContainer)
import Container.Root           (RootContainer)
import Control.Exception        (SomeException, try)
import Control.Monad.Cont       (runContT, ContT(..))
import Environment.Env          (withEnv)
import Network.HTTP.Types       (status404)
import Network.Wai              ( Application
                                , responseLBS
                                , pathInfo
                                , requestMethod
                                )
import Network.Wai.Handler.Warp (run)
import Platform.Web.Middleware  (withMiddleware)
import Platform.Web.Router      (appRoutes)

loadDotEnv :: IO ()
loadDotEnv = do
  _ <- try $ loadFile defaultConfig :: IO (Either SomeException ())
  return ()

start :: IO ()
start = do
  loadDotEnv
  cfg <- loadConfig
  withApp cfg $ run (port . webConf $ cfg) 

app :: RootContainer -> Application
app ctn req res = do
  case appRoutes ctn req mth pth of
    Just r  -> r res 
    Nothing -> notFound req res
  where 
    pth = pathInfo req
    mth = requestMethod req
  
notFound :: Application
notFound _ res = res $ responseLBS status404 [] "Not Found" 

withApp :: AppConfig -> (Application -> IO r) -> IO r
withApp cfg = runContT $ do
  ev <- ContT $ withEnv cfg
  ct <- ContT $ withRootContainer ev
  md <- ContT $ withMiddleware ev ct
  return . md . app $ ct 
