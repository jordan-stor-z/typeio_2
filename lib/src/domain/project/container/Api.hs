module Domain.Project.Container.Api where

import Database.Persist.Sql                   (ConnectionPool)
import Domain.Project.Responder.Node          (handleGetNodes)
import Domain.Project.Responder.NodeStatus    (handleGetNodeStatuses)
import Domain.Project.Responder.NodeType      (handleGetNodeTypes)
import Domain.Project.Responder.Project       (handleGetProjects)
import Domain.Project.Responder.Api.Node.Post (handlePostNode)
import Network.Wai                            (Application
                                              , Response
                                              , ResponseReceived
                                              )

data ProjectApiContainer = ProjectApiContainer
  { apiGetNodes        :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , apiGetNodeStatuses :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , apiGetNodeTypes    :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , apiGetProjects     :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , apiPostNode        :: Application
  }

defaultContainer :: ConnectionPool -> ProjectApiContainer
defaultContainer cpl = ProjectApiContainer
  { apiGetNodes        = handleGetNodes cpl 
  , apiGetNodeStatuses = handleGetNodeStatuses cpl  
  , apiGetNodeTypes    = handleGetNodeTypes cpl 
  , apiGetProjects     = handleGetProjects cpl 
  , apiPostNode        = handlePostNode cpl 
  }

