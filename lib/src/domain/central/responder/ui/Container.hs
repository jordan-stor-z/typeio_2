module Domain.Central.Responder.Ui.Container where

import Data.Text                          (Text)
import Domain.Central.Responder.Ui.IndexView (handleIndexView)
import Domain.Central.Responder.Ui.Empty  (handleGetEmpty)
import Network.Wai                        (Application, Response, ResponseReceived)

data Container = Container 
  { indexView :: Text -> Application 
  , emptyView :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  }

defaultContainer :: Container 
defaultContainer  = Container 
  { indexView = handleIndexView
  , emptyView = handleGetEmpty
  }
