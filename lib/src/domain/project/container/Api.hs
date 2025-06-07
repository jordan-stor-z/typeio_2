module Domain.Project.Container.Api where

import Common.Web.Types (Responder)

data ProjectApiContainer = ProjectApiContainer
  { apiGetNodes        :: Responder
  , apiGetNodeStatuses :: Responder
  , apiGetNodeTypes    :: Responder
  , apiGetProjects     :: Responder
  }
