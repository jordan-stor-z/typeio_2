{-# LANGUAGE OverloadedStrings #-}

module Platform.Web (start) where

import Config.App               (AppConfig(..), loadConfig)
import Config.Web               (WebConfig(..))
import Configuration.Dotenv     (defaultConfig, loadFile)
import Container.Build          (withRootContainer)
import Container.Root           (RootContainer)
import Control.Exception        (SomeException, try)
import Control.Monad.Cont       (runContT, ContT(..))
import Database.Persist.Sql     (runSqlPool, runMigration)
import Domain.Project.Model     (migrateAll)
import Environment.Env          (Env(..), withEnv)
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
  withApp cfg $ run (port . web $ cfg) 

app :: Env -> RootContainer -> Application
app ev ct req respond = do
  case appRoutes ct req mth pth of
    Just r  ->  r respond
    Nothing -> notFound req respond 
  where 
    pth = pathInfo req
    mth = requestMethod req
  
notFound :: Application
notFound _ respond = respond $ responseLBS status404 [] "Not Found" 

withApp :: AppConfig -> (Application -> IO r) -> IO r
withApp cf = runContT $ do
  ev <- ContT $ withEnv cf 
  ct <- ContT $ withRootContainer ev
  md <- ContT $ withMiddleware ev ct
  return $ md . app ev $ ct 
