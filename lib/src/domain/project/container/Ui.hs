module Domain.Project.Container.Ui where

import Network.Wai (Response, ResponseReceived)

data ProjectUiContainer = ProjectUiContainer
  { projectIndexVw ::  (Response -> IO ResponseReceived) -> IO ResponseReceived 
  , projectList    ::  (Response -> IO ResponseReceived) -> IO ResponseReceived
  , createProjectVw :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  }
