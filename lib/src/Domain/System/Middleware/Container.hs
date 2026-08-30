module Domain.System.Middleware.Container where

import Config.Web                                (WebConfig)
import Domain.System.Middleware.Logging.Request  (requestLogMiddleware)
import Domain.System.Middleware.Logging.Response (responseLogMiddleware)
import Domain.System.Middleware.RequestId        (requestIdMiddleware)
import Logging.Core                              (EntryLog)
import Network.Wai                               (Middleware)

data Container = Container 
  { logRequest   :: Middleware
  , logResponse  :: Middleware
  , tagRequestId :: Middleware
  }

defaultContainer :: WebConfig -> EntryLog -> Container 
defaultContainer webConf entryLog = Container 
  { logRequest   = requestLogMiddleware  webConf entryLog
  , logResponse  = responseLogMiddleware webConf entryLog
  , tagRequestId = requestIdMiddleware   webConf
  }
