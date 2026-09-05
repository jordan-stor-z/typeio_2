module Domain.Project.Responder.Ui.Container where

import Config.Visualization (Visualization (..))
import Database.Persist.Sql (ConnectionPool)
import Domain.Project.Responder.Ui.ProjectCreate.Submit (handleProjectSubmit)
import Domain.Project.Responder.Ui.ProjectCreate.View (handleProjectCreateVw)
import Domain.Project.Responder.Ui.ProjectIndex.List (handleProjectList)
import Domain.Project.Responder.Ui.ProjectIndex.View (handleProjectView)
import Domain.Project.Responder.Ui.ProjectManage.Node (handleGetNodePanel)
import Domain.Project.Responder.Ui.ProjectManage.Node.Description (handlePutDescription)
import Domain.Project.Responder.Ui.ProjectManage.Node.Detail (handleGetNodeDetail)
import Domain.Project.Responder.Ui.ProjectManage.Node.Edit (handleGetNodeEdit)
import Domain.Project.Responder.Ui.ProjectManage.Node.Refresh (handleGetNodeRefresh)
import Domain.Project.Responder.Ui.ProjectManage.Node.Status (handlePutNodeStatus)
import Domain.Project.Responder.Ui.ProjectManage.Node.Title (handlePutTitle)
import Domain.Project.Responder.Ui.ProjectManage.View (handleProjectManageView)
import qualified Domain.Project.Visualization.Layered.Responder as Layered
import qualified Domain.Project.Visualization.Orbital.Responder as Orbital
import qualified Domain.Project.Visualization.Rootless.Responder as Rootless
import Network.Wai
  ( Application
  , Response
  , ResponseReceived
  )

data Container = Container
  { projectIndexVw :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , projectList :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , createProjectVw :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , manageProjectVw :: Application
  , getProjectGraph :: Application
  , getNodeDetail :: Application
  , getNodeEdit :: Application
  , getNodePanel :: Application
  , getNodeRefresh :: Application
  , putNodeDescription :: Application
  , putNodeStatus :: Application
  , putNodeTitle :: Application
  , submitProject :: Application
  }

{- | The one place a visualization is selected.

Bound once, here, when the container is built — not branched on per
request, and not chosen by a query parameter. Everything downstream of
this holds a single 'Application' and is unaware there was a choice.
See @docs/architecture/visualization-switching.md@.
-}
graphHandler :: Visualization -> ConnectionPool -> Application
graphHandler Layered = Layered.handleProjectGraph
graphHandler Rootless = Rootless.handleProjectGraph
graphHandler Orbital = Orbital.handleProjectGraph

defaultContainer :: Visualization -> ConnectionPool -> Container
defaultContainer viz pl =
  Container
    { projectIndexVw = handleProjectView
    , projectList = handleProjectList pl
    , createProjectVw = handleProjectCreateVw
    , manageProjectVw = handleProjectManageView
    , getProjectGraph = graphHandler viz pl
    , getNodeDetail = handleGetNodeDetail pl
    , getNodeEdit = handleGetNodeEdit pl
    , getNodePanel = handleGetNodePanel
    , getNodeRefresh = handleGetNodeRefresh pl
    , putNodeDescription = handlePutDescription pl
    , putNodeStatus = handlePutNodeStatus pl
    , putNodeTitle = handlePutTitle pl
    , submitProject = handleProjectSubmit pl
    }
