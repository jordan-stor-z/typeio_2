module Domain.Central.Container.Api where

import Common.Web.Types (Responder)

newtype CentralApiContainer = CentralApiContainer 
  { apiSeedDatabase :: Responder 
  }
