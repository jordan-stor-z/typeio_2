module Container.Root where

import Config.App                         (AppConfig(..))
import Domain.Central.Container.Api       (CentralApiContainer)
import Domain.Central.Container.Ui        (CentralUiContainer)
import Domain.Project.Container.Api       (ProjectApiContainer)
import Domain.Project.Container.Ui        (ProjectUiContainer)
import Domain.System.Container.Api        (SystemApiContainer)
import Domain.System.Container.Middleware (SystemMiddlewareContainer)

data RootContainer = RootContainer
  { appConfig                 :: AppConfig
  , centralApiContainer       :: CentralApiContainer
  , centralUiContainer        :: CentralUiContainer 
  , projectApiContainer       :: ProjectApiContainer 
  , projectUiContainer        :: ProjectUiContainer
  , systemApiContainer        :: SystemApiContainer
  , systemMiddlewareContainer :: SystemMiddlewareContainer
  }

