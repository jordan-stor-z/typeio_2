module Domain.Central.Container.Ui where

import Data.Text (Text)
import Network.Wai (Response, ResponseReceived)

newtype CentralUiContainer = CentralUiContainer 
  { indexView :: Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
  }
