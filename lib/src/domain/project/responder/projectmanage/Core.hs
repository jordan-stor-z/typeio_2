{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.Core where

import Common.Validation      ( (.$)
                              , isThere 
                              , isNotEmpty 
                              , runValidation
                              , ValidationErr
                              , valRead
                              )
import Common.Web.Query       (lookupVal)
import Data.Int               (Int64)
import Data.Text              (unpack)
import Network.HTTP.Types.URI (QueryText)

queryProjectId :: QueryText -> Either [ValidationErr] Int64
queryProjectId qt = runValidation id $ do
  lookupVal "projectId" qt 
    .$ unpack
    >>= isThere    "Project id must be present" 
    >>= isNotEmpty "Project id must have a value"
    >>= valRead    "Project id must be valid integer" 
