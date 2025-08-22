module Domain.System.Container where

import Config.App (AppConfig, webConf)
import Domain.System.Middleware.Container as MC
import Domain.System.Responder.Container  as RC
import Domain.System.Middleware.RequestId (requestIdMiddleware)
import Domain.System.Middleware.Logging.Request  (requestLogMiddleware)
import Domain.System.Middleware.Logging.Response (responseLogMiddleware)
import Logging.Core                              (EntryLog)
import Network.Wai                               (Middleware)

data SystemContainer = SystemContainer
  { middleware :: MC.Container
  , responder  :: RC.Container
  }

defaultContainer :: AppConfig -> EntryLog -> SystemContainer 
defaultContainer cfg lg = SystemContainer
  { middleware = MC.defaultContainer wcfg lg
  , responder  = RC.defaultContainer cfg
  }
  where wcfg = webConf cfg
