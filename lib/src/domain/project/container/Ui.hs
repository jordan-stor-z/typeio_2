module Domain.Project.Container.Ui where

import Database.Persist.Sql                                    (ConnectionPool)
import Domain.Project.Responder.Ui.ProjectIndex.List              (handleProjectList)
import Domain.Project.Responder.Ui.ProjectIndex.View              (handleProjectView)
import Domain.Project.Responder.Ui.ProjectCreate.Submit           (handleProjectSubmit) 
import Domain.Project.Responder.Ui.ProjectCreate.View             (handleProjectCreateVw)
import Domain.Project.Responder.Ui.ProjectManage.Node             (handleGetNodePanel)
import Domain.Project.Responder.Ui.ProjectManage.Node.Description (handlePutDescription)
import Domain.Project.Responder.Ui.ProjectManage.Node.Detail      (handleGetNodeDetail)
import Domain.Project.Responder.Ui.ProjectManage.Node.Edit        (handleGetNodeEdit)
import Domain.Project.Responder.Ui.ProjectManage.Node.Status      (handlePutNodeStatus)
import Domain.Project.Responder.Ui.ProjectManage.Node.Title       (handlePutTitle)
import Domain.Project.Responder.Ui.ProjectManage.View             (handleProjectManageView)
import Domain.Project.Responder.Ui.ProjectManage.Graph            (handleProjectGraph)
import Domain.Project.Responder.Ui.ProjectManage.Node.Refresh     (handleGetNodeRefresh)
import Network.Wai                                             ( Application
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
  , getNodeRefresh     :: Application
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
  , getNodeRefresh     = handleGetNodeRefresh cpl
  , putNodeDescription = handlePutDescription cpl
  , putNodeStatus      = handlePutNodeStatus  cpl
  , putNodeTitle       = handlePutTitle       cpl
  , submitProject      = handleProjectSubmit  cpl
  }
