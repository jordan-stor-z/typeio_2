module Container.Root where

import Config.App                             (AppConfig(..))
import Domain.Central.Container               (CentralContainer)
import Domain.Project.Container               (ProjectContainer)
import Domain.System.Container                (SystemContainer)

data RootContainer = RootContainer
  { appConfig                 :: AppConfig
  , central                   :: CentralContainer
  , project                   :: ProjectContainer
  , system                    :: SystemContainer
  }

