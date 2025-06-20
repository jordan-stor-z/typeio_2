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
import Platform.Web.Router      (appRoutes, rootTree)

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
  let t = rootTree ctn req
  print "ROUTES ::::::"
  print t
  appRoutes ctn req res
  
withApp :: AppConfig -> (Application -> IO r) -> IO r
withApp cfg = runContT $ do
  ev <- ContT $ withEnv cfg
  ct <- ContT $ withRootContainer ev
  md <- ContT $ withMiddleware ev ct
  return . md . app $ ct 
