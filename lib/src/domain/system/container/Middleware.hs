module Domain.System.Container.Middleware where

import Network.Wai (Middleware)

data SystemMiddlewareContainer = SystemMiddlewareContainer
  { logRequest   :: Middleware
  , logResponse  :: Middleware
  , tagRequestId :: Middleware
  }
