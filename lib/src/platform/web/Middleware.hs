module Platform.Web.Middleware where

import Config.App                                (AppConfig(..))
import Container.Root                            (RootContainer(..))
import Environment.Env                           (Env(..))
import Domain.Central.Middleware.IndexRender     (renderIndexMiddleware)
import Domain.System.Middleware.RequestId        (requestIdMiddleware)
import Domain.System.Middleware.Logging.Request  (requestLogMiddleware)
import Domain.System.Middleware.Logging.Response (responseLogMiddleware)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static (static)

withMiddleware :: Env -> RootContainer -> (Middleware -> a) -> a
withMiddleware ev ct k = k (allMiddleware ev ct)

allMiddleware :: Env -> RootContainer -> Middleware
allMiddleware ev ct = 
  let m = 
        [ requestIdMiddleware wc
        , requestLogMiddleware wc lg
        , responseLogMiddleware wc lg
        , renderIndexMiddleware (centralUiContainer ct)
        , static
        ]
  in foldr1 (.) m
  where 
    cf = appConf ev
    lg = logger  ev
    wc = webConf cf

