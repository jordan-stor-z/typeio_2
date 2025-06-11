module Domain.Project.Container.Api where

import Network.Wai (Response, ResponseReceived)

data ProjectApiContainer = ProjectApiContainer
  { apiGetNodes        :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , apiGetNodeStatuses :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , apiGetNodeTypes    :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , apiGetProjects     :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  }
