{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Platform.Web.Router where

import Config.App                   (webDefaultPath)
import Common.Either ((-<), (<+>), (-|), Tree(..), addT, emptyT, findPath)
import Container.Root               (RootContainer(..))
import Data.Text                    (pack, Text)
import Domain.Central.Container.Api (CentralApiContainer(..))
import Domain.Central.Container.Ui  (CentralUiContainer(..))
import Domain.Project.Container.Api (ProjectApiContainer(..))
import Domain.Project.Container.Ui  (ProjectUiContainer(..))
import Domain.System.Container.Api  (SystemApiContainer(..))
import Control.Applicative ((<|>))
import Network.HTTP.Types           (status404)
import Network.Wai                  (Application, pathInfo, Request, Response, ResponseReceived, responseLBS)

appRoutes :: RootContainer 
  -> Request 
  -> ((Response -> IO ResponseReceived) -> IO ResponseReceived)
appRoutes ctn req = case findPath pth $ rootTree ctn req of
  Nothing -> notFound req
  Just r  -> r
  where pth = pathInfo req <|> [""]

notFound :: Application
notFound _ res = res $ responseLBS status404 [] "Not Found" 

rootTree :: RootContainer 
  -> Request
  -> Tree ((Response -> IO ResponseReceived) -> IO ResponseReceived)
rootTree ctn req = emptyT 
  <+> ""    -| index ctn dp
  <+> "api" -< apiTree ctn req
  <+> "ui"  -< uiTree  ctn req
  where dp = pack . webDefaultPath . appConfig $ ctn

apiTree :: RootContainer
  -> Request
  -> Tree ((Response -> IO ResponseReceived) -> IO ResponseReceived)
apiTree ctn req = emptyT
  <+> "central" -< centralApiTree ctrCtn req
  <+> "project" -< projectApiTree prjCtn req 
  <+> "system"  -< systemApiTree  sysCtn req 
  where
    ctrCtn = centralApiContainer ctn
    prjCtn = projectApiContainer ctn
    sysCtn = systemApiContainer  ctn

centralApiTree :: CentralApiContainer
  -> Request
  -> Tree ((Response -> IO ResponseReceived) -> IO ResponseReceived)
centralApiTree ctn req = emptyT
  <+> "seed-database" -| apiSeedDatabase ctn

projectApiTree :: ProjectApiContainer
  -> Request
  -> Tree ((Response -> IO ResponseReceived) -> IO ResponseReceived)
projectApiTree ctn req = emptyT
  <+> "nodes"         -| apiGetNodes ctn
  <+> "node-types"    -| apiGetNodeTypes ctn
  <+> "node-statuses" -| apiGetNodeStatuses ctn
  <+> "projects"      -| apiGetProjects ctn

systemApiTree :: SystemApiContainer
  -> Request
  -> Tree ((Response -> IO ResponseReceived) -> IO ResponseReceived)
systemApiTree ctn req = emptyT
  <+> "config" -| apiGetConfig ctn

uiTree :: RootContainer
  -> Request
  -> Tree ((Response -> IO ResponseReceived) -> IO ResponseReceived)
uiTree ctn req = emptyT
  <+> "projects"       -< projectIndexUiTree prjCtn req
  <+> "create-project" -< addProjectUiTree prjCtn req
  <+> "project"        -< manageProjectUiTree prjCtn req 
  where
    prjCtn = projectUiContainer ctn

projectIndexUiTree :: ProjectUiContainer
  -> Request
  -> Tree ((Response -> IO ResponseReceived) -> IO ResponseReceived)
projectIndexUiTree ctn req = emptyT
  <+> "vw"   -| projectIndexVw ctn
  <+> "list" -| projectList ctn

addProjectUiTree :: ProjectUiContainer
  -> Request
  -> Tree ((Response -> IO ResponseReceived) -> IO ResponseReceived)
addProjectUiTree ctn req = emptyT
  <+> "vw"     -| createProjectVw ctn
  <+> "submit" -| submitProject ctn req

manageProjectUiTree :: ProjectUiContainer
  -> Request
  -> Tree ((Response -> IO ResponseReceived) -> IO ResponseReceived)
manageProjectUiTree ctn req = emptyT
  <+> "vw" -| manageProjectVw ctn req

index :: RootContainer -> Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
index = indexView . centralUiContainer 

