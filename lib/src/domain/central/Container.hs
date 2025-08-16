module Domain.Central.Container where

import Database.Persist.Sql (ConnectionPool)
import qualified Domain.Central.Responder.Api.Container as API
import qualified Domain.Central.Responder.Ui.Container as UI 

data CentralContainer = CentralContainer
  { centralApiContainer :: API.CentralApiContainer
  , centralUiContainer  :: UI.Container
  }

defaultContainer :: ConnectionPool -> CentralContainer
defaultContainer pl = CentralContainer
  { centralApiContainer = API.defaultContainer pl
  , centralUiContainer  = UI.defaultContainer
  }
