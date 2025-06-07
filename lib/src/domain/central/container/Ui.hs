module Domain.Central.Container.Ui where

import Common.Web.Types (Responder)
import Data.Text (Text)

newtype CentralUiContainer = CentralUiContainer 
  { indexView :: Text -> Responder
  }
