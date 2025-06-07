{-# LANGUAGE OverloadedStrings #-}

module Domain.Central.Responder.Seed where

import Common.Web.Types            (Responder)
import Database.Persist            (insertUnique)
import Database.Persist.Sql        (runSqlPool)
import Database.Persist.Postgresql (ConnectionPool)
import Domain.Project.Model        (NodeStatus(..), NodeType(..))
import Network.HTTP.Types          (status200)
import Network.Wai                 (responseLBS)

handleSeedDatabase :: ConnectionPool -> Responder  
handleSeedDatabase pool respond = do
  flip runSqlPool pool $ do
    mapM_ insertUnique nodeStatuses
    mapM_ insertUnique nodeTypes
  respond $ responseLBS 
    status200 
    [("Content-Type", "application/json")] 
    "Database seeded successfully"

nodeTypes :: [NodeType]
nodeTypes =
  [ NodeType "project_root"
  , NodeType "work"
  ]

nodeStatuses :: [NodeStatus]
nodeStatuses =
  [ NodeStatus "active"
  , NodeStatus "closed"
  , NodeStatus "open"
  , NodeStatus "rejected"
  ]

