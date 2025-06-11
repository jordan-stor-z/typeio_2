module Domain.Central.Container.Api where

import Network.Wai (Response, ResponseReceived)

newtype CentralApiContainer = CentralApiContainer 
  { apiSeedDatabase :: (Response -> IO ResponseReceived) -> IO ResponseReceived 
  }
