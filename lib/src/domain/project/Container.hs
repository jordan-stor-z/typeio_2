module Domain.Project.Container where

import Database.Persist.Sql (ConnectionPool)
import qualified Domain.Project.Container.Api as Api
import qualified Domain.Project.Responder.Ui.Container as Ui

data ProjectContainer = ProjectContainer
  { projectApiContainer' :: Api.ProjectApiContainer
  , projectUiContainer'  :: Ui.Container
  }

defaultContainer :: ConnectionPool -> ProjectContainer
defaultContainer pl = ProjectContainer
  { projectApiContainer' = Api.defaultContainer pl
  , projectUiContainer'  = Ui.defaultContainer  pl
  }
