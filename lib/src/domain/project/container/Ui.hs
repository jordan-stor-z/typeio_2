module Domain.Project.Container.Ui where

import Data.Text   (Text)
import Network.Wai (Application, Response, ResponseReceived)

data ProjectUiContainer = ProjectUiContainer
  { projectIndexVw  :: (Response -> IO ResponseReceived) -> IO ResponseReceived 
  , projectList     :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , createProjectVw :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , manageProjectVw :: Application
  , submitProject   :: Application 
  }
