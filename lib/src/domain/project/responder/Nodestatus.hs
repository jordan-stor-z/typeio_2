{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Project.Responder.NodeStatus where

import Data.Aeson                          ((.=)
                                           , encode
                                           , toJSON
                                           , ToJSON
                                           , object
                                           )
import Database.Esqueleto.Experimental     (from, select, table)
import Database.Persist                    (Entity(..))
import Database.Persist.Sql                (ConnectionPool, runSqlPool)
import qualified Domain.Project.Model as M (NodeStatus(..), unNodeStatusKey)
import Network.HTTP.Types                  (status200)
import Network.Wai                         (Response, responseLBS, ResponseReceived)

newtype NodeStatus = NodeStatus
  { nodeStatusId :: String
  }

instance ToJSON NodeStatus where
  toJSON (NodeStatus ntId) =
    object [ "nodeStatusId" .= ntId ]

handleGetNodeStatuses :: ConnectionPool -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGetNodeStatuses pl respond = do
  ns <- encode . map toSchema <$> runSqlPool query pl 
  respond $ responseLBS status200 [("Content-Type", "application/json")] ns
  where
    query = select $ from $ table @M.NodeStatus
    toSchema (Entity k _) = NodeStatus
      { nodeStatusId = M.unNodeStatusKey k
      }

