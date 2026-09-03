module Domain.Project.Container where

import Config.Visualization (Visualization)
import Database.Persist.Sql (ConnectionPool)
import qualified Domain.Project.Responder.Api.Container as Api
import qualified Domain.Project.Responder.Ui.Container as Ui

data ProjectContainer = ProjectContainer
  { projectApiContainer' :: Api.Container
  , projectUiContainer' :: Ui.Container
  }

{- | Takes the selected 'Visualization' rather than the whole
'Config.App.AppConfig': the UI container needs exactly one field of it,
and the container pattern is about handing each level only what its
handlers actually use.
-}
defaultContainer :: Visualization -> ConnectionPool -> ProjectContainer
defaultContainer viz pl =
  ProjectContainer
    { projectApiContainer' = Api.defaultContainer pl
    , projectUiContainer' = Ui.defaultContainer viz pl
    }
