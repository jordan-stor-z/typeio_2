module Domain.System.Container.Api where

import Config.App                     (AppConfig)
import Domain.System.Responder.Config (handleGetConfig)
import Network.Wai                    (Response, ResponseReceived)

newtype SystemApiContainer = SystemApiContainer
  { apiGetConfig :: (Response -> IO ResponseReceived) -> IO ResponseReceived 
  }

defaultContainer :: AppConfig -> SystemApiContainer
defaultContainer cf = SystemApiContainer
  { apiGetConfig = handleGetConfig cf
  }

