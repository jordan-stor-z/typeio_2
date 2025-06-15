module Container.Build where

import Config.App                                  (AppConfig(..))
import Container.Root                              (RootContainer(..))
import Domain.Central.Container.Api                (CentralApiContainer(..))
import Domain.Central.Container.Ui                 (CentralUiContainer(..))
import Domain.Central.Responder.IndexView          (handleIndexView)
import Domain.Central.Responder.Seed               (handleSeedDatabase)
import Domain.Project.Responder.Node               (handleGetNodes)
import Domain.Project.Responder.NodeStatus         (handleGetNodeStatuses)
import Domain.Project.Responder.NodeType           (handleGetNodeTypes)
import Domain.Project.Responder.Project            (handleGetProjects)
import Domain.Project.Responder.ProjectIndex.List  (handleProjectList)
import Domain.Project.Responder.ProjectIndex.View  (handleProjectView)
import Domain.Project.Responder.ProjectCreate.Submit (handleProjectSubmit)
import Domain.Project.Responder.ProjectCreate.View (handleProjectCreateVw)
import Domain.Project.Container.Api                (ProjectApiContainer(..))
import Domain.Project.Container.Ui                 (ProjectUiContainer(..))
import Domain.System.Container.Api                 (SystemApiContainer(..))
import Domain.System.Container.Middleware          (SystemMiddlewareContainer(..))
import Domain.System.Middleware.RequestId          (requestIdMiddleware)
import Domain.System.Middleware.Logging.Request    (requestLogMiddleware)
import Domain.System.Middleware.Logging.Response   (responseLogMiddleware)
import Domain.System.Responder.Config              (handleGetConfig)
import Environment.Env                             (Env(..))

withRootContainer :: Env -> (RootContainer -> a) -> a 
withRootContainer ev k = 
  let centralApi = CentralApiContainer
        { apiSeedDatabase = handleSeedDatabase pl 
        } 
      centralUi  = CentralUiContainer
        { indexView = handleIndexView
        }
      projectApi = ProjectApiContainer
        { apiGetNodes        = handleGetNodes pl 
        , apiGetNodeStatuses = handleGetNodeStatuses pl  
        , apiGetNodeTypes    = handleGetNodeTypes pl 
        , apiGetProjects     = handleGetProjects pl 
        }
      projectUi  = ProjectUiContainer
        { projectIndexVw     = handleProjectView
        , projectList        = handleProjectList pl lg
        , createProjectVw    = handleProjectCreateVw 
        , submitProject      = handleProjectSubmit pl
        }
      systemApi  = SystemApiContainer
        { apiGetConfig = handleGetConfig cf 
        }
      systemMiddleware = SystemMiddlewareContainer
        { logRequest = requestLogMiddleware   (web cf) lg 
        , logResponse = responseLogMiddleware (web cf) lg
        , tagRequestId = requestIdMiddleware  (web cf) 
        }
      root       = RootContainer
        { appConfig            = cf
        , centralApiContainer  = centralApi 
        , centralUiContainer   = centralUi
        , projectApiContainer  = projectApi
        , projectUiContainer   = projectUi
        , systemApiContainer   = systemApi
        , systemMiddlewareContainer = systemMiddleware
        }
  in k root
  where 
    cf = config ev
    lg = logger ev
    pl = pool ev


