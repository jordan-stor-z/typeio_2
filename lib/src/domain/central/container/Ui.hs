module Domain.Central.Container.Ui where

import Data.Text                          (Text)
import Domain.Central.Responder.IndexView (handleIndexView)
import Network.Wai                        (Application)

newtype CentralUiContainer = CentralUiContainer 
  { indexView :: Text -> Application 
  }

defaultContainer :: CentralUiContainer
defaultContainer  = CentralUiContainer
  { indexView = handleIndexView
  }
