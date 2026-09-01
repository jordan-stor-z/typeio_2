module Domain.System.Container where

import Config.App (AppConfig, webConf)
import Domain.System.Middleware.Container as MC
import Domain.System.Responder.Container as RC
import Logging.Core (EntryLog)

data SystemContainer = SystemContainer
  { middleware :: MC.Container
  , responder :: RC.Container
  }

defaultContainer :: AppConfig -> EntryLog -> SystemContainer
defaultContainer cfg lg =
  SystemContainer
    { middleware = MC.defaultContainer wcfg lg
    , responder = RC.defaultContainer cfg
    }
  where
    wcfg = webConf cfg
