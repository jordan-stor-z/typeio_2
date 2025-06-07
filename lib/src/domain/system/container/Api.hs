module Domain.System.Container.Api where

import Common.Web.Types (Responder)

newtype SystemApiContainer = SystemApiContainer
  { apiGetConfig :: Responder 
  }
