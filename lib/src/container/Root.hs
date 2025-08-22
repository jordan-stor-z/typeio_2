module Container.Root where

import Config.App                             (AppConfig(..))
import Domain.Central.Container               (CentralContainer)
import Domain.Project.Container               (ProjectContainer)
import Domain.Project.Container.Api           (ProjectApiContainer)
import Domain.Project.Container.Ui            (ProjectUiContainer)
import Domain.System.Container.Api            (SystemApiContainer)
import Domain.System.Container.Middleware     (SystemMiddlewareContainer)

data RootContainer = RootContainer
  { appConfig                 :: AppConfig
  , central                   :: CentralContainer
  , project                   :: ProjectContainer
  , projectApiContainer       :: ProjectApiContainer 
  , projectUiContainer        :: ProjectUiContainer
  , systemApiContainer        :: SystemApiContainer
  , systemMiddlewareContainer :: SystemMiddlewareContainer
  }

