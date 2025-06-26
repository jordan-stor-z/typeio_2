module Domain.Central.Container.Ui where

import Data.Text                          (Text)
import Domain.Central.Responder.IndexView (handleIndexView)
import Network.Wai                        (Response, ResponseReceived)

newtype CentralUiContainer = CentralUiContainer 
  { indexView :: Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
  }

defaultContainer :: CentralUiContainer
defaultContainer  = CentralUiContainer
  { indexView = handleIndexView
  }
