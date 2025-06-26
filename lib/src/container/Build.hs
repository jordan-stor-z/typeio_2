module Container.Build where

import Config.App      (AppConfig(..))
import Container.Root  (RootContainer(..))
import Environment.Env (Env(..))
import qualified Domain.Central.Container.Api as CA
import qualified Domain.Central.Container.Ui  as CU
import qualified Domain.Project.Container.Api as PA                 
import qualified Domain.Project.Container.Ui  as PU
import qualified Domain.System.Container.Api  as SA
import qualified Domain.System.Container.Middleware as SM 

withRootContainer :: Env -> (RootContainer -> IO a) -> IO a
withRootContainer ev k = k RootContainer
  { appConfig                 = appConf ev
  , centralApiContainer       = CA.defaultContainer  pl 
  , centralUiContainer        = CU.defaultContainer
  , projectApiContainer       = PA.defaultContainer  pl
  , projectUiContainer        = PU.defaultContainer  pl
  , systemApiContainer        = SA.defaultContainer (appConf ev) 
  , systemMiddlewareContainer = SM.defaultContainer (webConf cf) lg 
  }
  where 
    cf = appConf ev
    lg = logger ev
    pl = pool ev

