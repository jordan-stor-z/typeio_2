module Common.Web.Types where

import Network.Wai (Response, ResponseReceived)

type Respond   = (Response -> IO ResponseReceived)
type Responder = Respond -> IO ResponseReceived

