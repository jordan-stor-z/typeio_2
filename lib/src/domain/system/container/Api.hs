module Domain.System.Container.Api where

import Network.Wai (Response, ResponseReceived)

newtype SystemApiContainer = SystemApiContainer
  { apiGetConfig :: (Response -> IO ResponseReceived) -> IO ResponseReceived 
  }
