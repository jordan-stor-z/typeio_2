{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Project.Responder.NodeType where

import Common.Web.Types                   (Responder)
import Data.Aeson                         ((.=)
                                           , encode
                                           , toJSON
                                           , ToJSON
                                           , object
                                           )
import Database.Esqueleto.Experimental     (from, select, table)
import Database.Persist                    (Entity(..))
import Database.Persist.Sql                (ConnectionPool, runSqlPool)
import qualified Domain.Project.Model as M (NodeType(..), unNodeTypeKey)
import Network.HTTP.Types                  (status200)
import Network.Wai                         (responseLBS)

newtype NodeType = NodeType
  { nodeTypeId :: String
  }

instance ToJSON NodeType where
  toJSON (NodeType ntId) =
    object [ "nodeTypeId" .= ntId ]

handleGetNodeTypes :: ConnectionPool -> Responder 
handleGetNodeTypes pl respond = do
  ns <- encode . map toSchema <$> runSqlPool query pl 
  respond $ responseLBS status200 [("Content-Type", "application/json")] ns
  where
    query = select $ from $ table @M.NodeType
    toSchema (Entity k _) = NodeType
      { nodeTypeId = M.unNodeTypeKey k
      }

