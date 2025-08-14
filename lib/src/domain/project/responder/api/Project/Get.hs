{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Project.Responder.Api.Project.Get where

import Data.Aeson                          ((.=)
                                           , encode
                                           , toJSON
                                           , ToJSON
                                           , object
                                           )
import Data.Int                            (Int64)
import Database.Esqueleto.Experimental     (from, select, table)
import Database.Persist                    (Entity(..))
import Database.Persist.Sql                (ConnectionPool, fromSqlKey, runSqlPool)
import qualified Domain.Project.Model as M (Project(..)) 
import Network.HTTP.Types                  (status200)
import Network.Wai                         (Response, responseLBS, ResponseReceived)

newtype Project = Project 
  { projectId :: Int64
  }

instance ToJSON Project where
  toJSON (Project pId) =
    object [ "projectId" .= pId ]

handleGetProjects :: ConnectionPool -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGetProjects pl respond = do
  ns <- encode . map toSchema <$> runSqlPool query pl 
  respond $ responseLBS status200 [("Content-Type", "application/json")] ns
  where
    query = select $ from $ table @M.Project
    toSchema (Entity k _) = Project
      { projectId = fromSqlKey k
      }
