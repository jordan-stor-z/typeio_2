{-# LANGUAGE OverloadedStrings #-}

module Domain.System.Responder.Config where

import Config.App         (AppConfig(..), Environment(..)) 
import Config.Db          (connStr, DbConfig(..))
import Data.Aeson         (encode, ToJSON, toJSON, object, (.=))
import Network.HTTP.Types (status200)
import Network.Wai        (responseLBS)
import Common.Web.Types   (Responder)

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

handleGetConfig :: AppConfig -> Responder 
handleGetConfig cf respond = do
  let e = env cf 
      cf' = preprocessConfig e cf
      cs' = maskField e (connStr . db $ cf')
      cd  = ConfigDisplay cf' cs'
      js  = encode cd
  respond $ responseLBS status200 [("Content-Type", "application/json")] js

maskField :: Environment -> String -> String
maskField Production _ = replicate 22 '*'
maskField _ field      = field

preprocessConfig :: Environment -> AppConfig -> AppConfig
preprocessConfig e cfg = cfg { db = db' }
  where
    d = db cfg
    db' = d { password = maskField e (password d) } 

