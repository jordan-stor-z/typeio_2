module Domain.Project.Container.Ui where

import Database.Persist.Sql (ConnectionPool)
import Domain.Project.Responder.ProjectIndex.List         (handleProjectList)
import Domain.Project.Responder.ProjectIndex.View         (handleProjectView)
import Domain.Project.Responder.ProjectCreate.Submit      (handleProjectSubmit)
import Domain.Project.Responder.ProjectCreate.View        (handleProjectCreateVw)
import Domain.Project.Responder.ProjectManage.Node        (handleGetNodePanel)
import Domain.Project.Responder.ProjectManage.Node.Detail (handleGetNodeDetail)
import Domain.Project.Responder.ProjectManage.Node.Edit   (handleGetNodeEdit) 
import Domain.Project.Responder.ProjectManage.Node.Save   (handlePutDescription)
import Domain.Project.Responder.ProjectManage.Node.Status (handlePutNodeStatus)
import Domain.Project.Responder.ProjectManage.Node.Title  (handlePutTitle)
import Domain.Project.Responder.ProjectManage.View        (handleProjectManageView)
import Domain.Project.Responder.ProjectManage.Graph       (handleProjectGraph)
import Network.Wai                                        ( Application
                                                          , Response
                                                          , ResponseReceived
                                                          )

data ProjectUiContainer = ProjectUiContainer
  { projectIndexVw     :: (Response -> IO ResponseReceived) -> IO ResponseReceived 
  , projectList        :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , createProjectVw    :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , manageProjectVw    :: Application
  , getProjectGraph    :: Application
  , getNodeDetail      :: Application
  , getNodeEdit        :: Application
  , getNodePanel       :: Application
  , putNodeDescription :: Application
  , putNodeStatus      :: Application
  , putNodeTitle       :: Application
  , submitProject      :: Application 
  }

defaultContainer :: ConnectionPool -> ProjectUiContainer
defaultContainer cpl = ProjectUiContainer
  { projectIndexVw     = handleProjectView
  , projectList        = handleProjectList cpl
  , createProjectVw    = handleProjectCreateVw 
  , manageProjectVw    = handleProjectManageView
  , getProjectGraph    = handleProjectGraph   cpl
  , getNodeDetail      = handleGetNodeDetail  cpl
  , getNodeEdit        = handleGetNodeEdit    cpl
  , getNodePanel       = handleGetNodePanel 
  , putNodeDescription = handlePutDescription cpl
  , putNodeStatus      = handlePutNodeStatus  cpl
  , putNodeTitle       = handlePutTitle       cpl
  , submitProject      = handleProjectSubmit  cpl
  }
