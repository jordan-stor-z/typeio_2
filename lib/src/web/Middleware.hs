module Web.Middleware where

import Config.App (AppConfig(..))
import Environment.Env (Env(..))
import Web.Middleware.RequestId (requestIdMiddleware)
import Web.Middleware.Logging.Request (requestLogMiddleware)
import Web.Middleware.Logging.Response (responseLogMiddleware)
import Network.Wai (Middleware)

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

