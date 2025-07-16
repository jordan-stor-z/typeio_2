module Domain.Central.Container.Ui where

import Data.Text                          (Text)
import Domain.Central.Responder.IndexView (handleIndexView)
import Domain.Central.Responder.Empty     (handleGetEmpty)
import Network.Wai                        (Application, Response, ResponseReceived)

data CentralUiContainer = CentralUiContainer 
  { indexView :: Text -> Application 
  , getEmptyView :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  }

defaultContainer :: CentralUiContainer
defaultContainer  = CentralUiContainer
  { indexView    = handleIndexView
  , getEmptyView = handleGetEmpty
  }
