{-# LANGUAGE OverloadedStrings #-}

module Domain.Admin.Application.Config where

import Config.App (AppConfig(..), Environment(..)) 
import Config.Db (connStr, DbConfig(..))
import Data.Aeson (encode, ToJSON, toJSON, object, (.=))
import Data.Text (Text)
import Environment.Env (Env(..))
import Network.HTTP.Types (Method, status200)
import Network.Wai (Application, responseLBS)

admin :: Env -> Method -> [Text] -> Maybe Application 
admin ev mt path = case (mt, path) of
    ("GET", ["config"]) -> Just $ handleGetConfig (config ev)
    _ -> Nothing

data ConfigDisplay = ConfigDisplay
  { configuration    :: AppConfig
  , connectionString :: String
  }

instance ToJSON ConfigDisplay where
  toJSON (ConfigDisplay cf cs) =
    object
      [ "config" .= cf
      , "connectionString" .= cs
      ]

handleGetConfig :: AppConfig -> Application
handleGetConfig cf _ respond = do
  let e = env cf 
      cf' = preprocessConfig e cf
      cs' = maskField e (connStr . db $ cf')
      cd = ConfigDisplay cf' cs'
      js = encode cd
  respond $ responseLBS status200 [("Content-Type", "application/json")] js

maskField :: Environment -> String -> String
maskField Production _ = replicate 22 '*'
maskField _ field = field

preprocessConfig :: Environment -> AppConfig -> AppConfig
preprocessConfig e cfg = cfg { db = db' }
  where
    d = db cfg
    db' = d { password = maskField e (password d) } 
