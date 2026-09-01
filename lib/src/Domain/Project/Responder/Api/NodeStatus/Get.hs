{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Project.Responder.Api.NodeStatus.Get where

import Data.Aeson
  ( ToJSON
  , encode
  , object
  , toJSON
  , (.=)
  )
import Database.Esqueleto.Experimental (from, select, table)
import Database.Persist (Entity (..))
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import qualified Domain.Project.Model as M (NodeStatus (..), unNodeStatusKey)
import Network.HTTP.Types (status200)
import Network.Wai (Response, ResponseReceived, responseLBS)

newtype NodeStatus = NodeStatus
  { nodeStatusId :: String
  }

instance ToJSON NodeStatus where
  toJSON (NodeStatus ntId) =
    object ["nodeStatusId" .= ntId]

handleGetNodeStatuses :: ConnectionPool -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGetNodeStatuses pl respond = do
  ns <- encode . map toSchema <$> runSqlPool query pl
  respond $ responseLBS status200 [("Content-Type", "application/json")] ns
  where
    query = select $ from $ table @M.NodeStatus
    toSchema (Entity k _) =
      NodeStatus
        { nodeStatusId = M.unNodeStatusKey k
        }
