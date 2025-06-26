{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Platform.Web.Router where

import Config.App                   (webDefaultPath)
import Common.Either ((-<), (<+>), (-|), Tree(..), addT, emptyT, findPath)
import Container.Root               (RootContainer(..))
import Data.Maybe                   (fromMaybe)
import Data.Text                    (pack, Text)
import Domain.Central.Container.Api (CentralApiContainer(..))
import Domain.Central.Container.Ui  (CentralUiContainer(..))
import Domain.Project.Container.Api (ProjectApiContainer(..))
import Domain.Project.Container.Ui  (ProjectUiContainer(..))
import Domain.System.Container.Api  (SystemApiContainer(..))
import Control.Applicative ((<|>))
import Network.HTTP.Types           (status404, Method)
import Network.Wai                  (Application, pathInfo, Request, requestMethod, Response, ResponseReceived, responseLBS)

type RouteTree = Tree Text MethodTree 
type MethodTree = 
  Tree Method ((Response -> IO ResponseReceived) -> IO ResponseReceived)

routes :: RouteTree
routes = emptyT

methods :: MethodTree
methods = emptyT

only :: Method 
  -> ((Response -> IO ResponseReceived) -> IO ResponseReceived) 
  -> MethodTree
only m r = methods <+> m -| r

routeRequest :: RootContainer 
  -> Request 
  -> ((Response -> IO ResponseReceived) -> IO ResponseReceived)
routeRequest ctn req = fromMaybe (notFound req) $
  findPath pth (rootTree ctn req) 
    >>= findPath [mth]
  where 
    pth = pathInfo req
    mth = requestMethod req

notFound :: Application
notFound _ res = res $ responseLBS status404 [] "Not Found" 

rootTree :: RootContainer -> Request -> RouteTree 
rootTree ctn req = emptyT 
  <+> ""    -| only "GET" (index ctn dp)
  <+> "api" -< apiTree ctn req
  <+> "ui"  -< uiTree  ctn req
  where dp = pack . webDefaultPath . appConfig $ ctn

apiTree :: RootContainer -> Request -> RouteTree
apiTree ctn req = emptyT
  <+> "central" -< centralApiTree ctrCtn
  <+> "project" -< projectApiTree prjCtn req 
  <+> "system"  -< systemApiTree  sysCtn req 
  where
    ctrCtn = centralApiContainer ctn
    prjCtn = projectApiContainer ctn
    sysCtn = systemApiContainer  ctn

centralApiTree :: CentralApiContainer -> RouteTree
centralApiTree ctn = emptyT
  <+> "seed-database" -| only "POST" (apiSeedDatabase ctn)

projectApiTree :: ProjectApiContainer -> Request -> RouteTree
projectApiTree ctn req = emptyT 
  <+> "nodes"    -| 
    ( methods 
      <+> "GET"  -| apiGetNodes ctn
      <+> "POST" -| apiPostNode ctn req
    )
  <+> "node-types"    -| only "GET" (apiGetNodeTypes ctn)
  <+> "node-statuses" -| only "GET" (apiGetNodeStatuses ctn)
  <+> "projects"      -| only "GET" (apiGetProjects ctn)


systemApiTree :: SystemApiContainer -> Request -> RouteTree
systemApiTree ctn _ = emptyT
  <+> "config" -| only "GET" (apiGetConfig ctn)

uiTree :: RootContainer -> Request -> RouteTree
uiTree ctn req = emptyT
  <+> "projects"       -< projectIndexUiTree prjCtn req
  <+> "create-project" -< addProjectUiTree prjCtn req
  <+> "project"        -< manageProjectUiTree prjCtn req 
  where
    prjCtn = projectUiContainer ctn

projectIndexUiTree :: ProjectUiContainer -> Request -> RouteTree
projectIndexUiTree ctn _ = emptyT
  <+> "vw"   -| only "GET" (projectIndexVw ctn)
  <+> "list" -| only "GET" (projectList ctn)

addProjectUiTree :: ProjectUiContainer -> Request -> RouteTree
addProjectUiTree ctn req = emptyT
  <+> "vw"     -| only "GET"  (createProjectVw ctn)
  <+> "submit" -| only "POST" (submitProject ctn req)

manageProjectUiTree :: ProjectUiContainer -> Request -> RouteTree
manageProjectUiTree ctn req = emptyT
  <+> "vw" -| only "GET" (manageProjectVw ctn req)

index :: RootContainer 
  -> Text 
  -> (Response -> IO ResponseReceived) -> IO ResponseReceived
index = indexView . centralUiContainer 

