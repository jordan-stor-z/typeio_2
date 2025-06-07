module Platform.Web.Middleware where

import Config.App (AppConfig(..))
import Environment.Env (Env(..))
import Domain.System.Middleware.RequestId (requestIdMiddleware)
import Domain.System.Middleware.Logging.Request (requestLogMiddleware)
import Domain.System.Middleware.Logging.Response (responseLogMiddleware)
import Network.Wai (Middleware)

withMiddleware :: Env -> (Middleware -> a) -> a
withMiddleware ev k = k (allMiddleware ev)

allMiddleware :: Env -> Middleware
allMiddleware ev = 
  let m = 
        [ requestIdMiddleware wc
        , requestLogMiddleware wc lg
        , responseLogMiddleware wc lg
        ]
  in foldr1 (.) m
  where 
    cf = config ev
    lg = logger ev
    wc = web cf

