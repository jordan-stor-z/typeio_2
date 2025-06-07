module Domain.Project.Container.Ui where

import Common.Web.Types (Responder)   

newtype ProjectUiContainer = ProjectUiContainer
  { uiProjectIndex :: Responder 
  }
