module Domain.System.Container.Middleware where

import Config.Web                                (WebConfig)
import Domain.System.Middleware.Logging.Request  (requestLogMiddleware)
import Domain.System.Middleware.Logging.Response (responseLogMiddleware)
import Domain.System.Middleware.RequestId        (requestIdMiddleware)
import Logging.Core                              (EntryLog)
import Network.Wai                               (Middleware)

data SystemMiddlewareContainer = SystemMiddlewareContainer
  { logRequest   :: Middleware
  , logResponse  :: Middleware
  , tagRequestId :: Middleware
  }

defaultContainer :: WebConfig -> EntryLog -> SystemMiddlewareContainer
defaultContainer webConf entryLog = SystemMiddlewareContainer
  { logRequest   = requestLogMiddleware webConf entryLog
  , logResponse  = responseLogMiddleware webConf entryLog
  , tagRequestId = requestIdMiddleware webConf
  }
