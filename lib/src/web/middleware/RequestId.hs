module Web.Middleware.RequestId where

import Config.Web (WebConfig(..))
import Network.Wai (Middleware, requestHeaders) 
import Data.UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)

requestIdMiddleware :: WebConfig -> Middleware
requestIdMiddleware wc ap req respond = do
  uid <- nextRandom
  let hd = (requestIdHeader wc, toASCIIBytes uid)
      nh = hd : requestHeaders req
      req' = req { requestHeaders = nh } 
  ap req' respond
