{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Project.Responder.Project where

import Data.Aeson                          ((.=)
                                           , encode
                                           , toJSON
                                           , ToJSON
                                           , object
                                           )
import Database.Esqueleto.Experimental     (from, select, table)
import Database.Persist                    (Entity(..))
import Database.Persist.Sql                (ConnectionPool, runSqlPool)
import qualified Domain.Project.Model as M (Project(..), unProjectKey) 
import Network.HTTP.Types                  (status200)
import Network.Wai                         (responseLBS)
import Common.Web.Types                    (Responder)

newtype Project = Project 
  { projectId :: Int 
  }

instance ToJSON Project where
  toJSON (Project pId) =
    object [ "projectId" .= pId ]

handleGetProjects :: ConnectionPool -> Responder
handleGetProjects pl respond = do
  ns <- encode . map toSchema <$> runSqlPool query pl 
  respond $ responseLBS status200 [("Content-Type", "application/json")] ns
  where
    query = select $ from $ table @M.Project
    toSchema (Entity k _) = Project
      { projectId = M.unProjectKey k
      }
