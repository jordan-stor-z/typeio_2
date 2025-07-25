module Domain.Central.Container.Api where

import Database.Persist.Sql          (ConnectionPool)
import Domain.Central.Responder.Seed (handleSeedDatabase)
import Network.Wai                   (Response, ResponseReceived)

newtype CentralApiContainer = CentralApiContainer 
  { apiSeedDatabase :: (Response -> IO ResponseReceived) -> IO ResponseReceived 
  }

defaultContainer :: ConnectionPool -> CentralApiContainer
defaultContainer cpl = CentralApiContainer
  { apiSeedDatabase = handleSeedDatabase cpl
  }
