module Platform.Web (main) where

import Config.App               (AppConfig(..), loadConfig)
import Config.Web               (WebConfig(..))
import Configuration.Dotenv     (defaultConfig, loadFile)
import Container.Build          (withRootContainer)
import Container.Root           (RootContainer)
import Control.Exception        (SomeException, try)
import Control.Monad.Cont       (runContT, ContT(..))
import Environment.Env          (withEnv)
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Platform.Web.Middleware  (withMiddleware)
import Platform.Web.Router      (routeRequest)

loadDotEnv :: IO ()
loadDotEnv = do
  _ <- try $ loadFile defaultConfig :: IO (Either SomeException ())
  return ()

main :: IO ()
main = do
  loadDotEnv
  cfg <- loadConfig
  withApp cfg $ run (pt cfg) 
  where pt = port . webConf

app :: RootContainer -> Application
app = routeRequest 
  
withApp :: AppConfig -> (Application -> IO r) -> IO r
withApp cfg = runContT $ do
  ev <- ContT $ withEnv cfg
  ct <- ContT $ withRootContainer ev
  md <- ContT $ withMiddleware ev ct
  return . md . app $ ct 
