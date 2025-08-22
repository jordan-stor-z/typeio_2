module Domain.Project.Responder.Api.Container where

import Database.Persist.Sql                        (ConnectionPool)
import Domain.Project.Responder.Api.Node.Get       (handleGetNodes)
import Domain.Project.Responder.Api.NodeStatus.Get (handleGetNodeStatuses)
import Domain.Project.Responder.Api.NodeType.Get   (handleGetNodeTypes)
import Domain.Project.Responder.Api.Project.Get    (handleGetProjects)
import Domain.Project.Responder.Api.Node.Post      (handlePostNode)
import Network.Wai                                 (Application
                                                  , Response
                                                  , ResponseReceived
                                                  )

data Container = Container 
  { getNodes        :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , getNodeStatuses :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , getNodeTypes    :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , getProjects     :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , postNode        :: Application
  }

defaultContainer :: ConnectionPool -> Container 
defaultContainer cpl = Container 
  { getNodes        = handleGetNodes cpl 
  , getNodeStatuses = handleGetNodeStatuses cpl  
  , getNodeTypes    = handleGetNodeTypes cpl 
  , getProjects     = handleGetProjects cpl 
  , postNode        = handlePostNode cpl 
  }

