module Domain.System.Responder.Container where

import Config.App                     (AppConfig)
import Domain.System.Responder.Config (handleGetConfig)
import Network.Wai                    (Response, ResponseReceived)

newtype Container = Container 
  { getConfig :: (Response -> IO ResponseReceived) -> IO ResponseReceived 
  }

defaultContainer :: AppConfig -> Container 
defaultContainer cf = Container 
  { getConfig = handleGetConfig cf
  }

