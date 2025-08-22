{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Platform.Web.Router where

import Config.App                             (webDefaultPath)
import Data.HashTree                          ( (-<)
                                              , (<+>)
                                              , (-|)
                                              , HashTree(..)
                                              , emptyT
                                              , findPath
                                              )
import Container.Root                         (RootContainer(..))
import Data.Maybe                             (fromMaybe)
import Data.Text                              (pack, Text)
import Domain.Central.Container               (CentralContainer(..))
import Domain.Central.Responder.Api.Container (CentralApiContainer(..))
import Domain.Project.Container.Api           (ProjectApiContainer(..))
import Domain.Project.Container.Ui            (ProjectUiContainer(..))
import qualified Domain.Project.Responder.Api.Container as PA
import qualified Domain.Project.Responder.Ui.Container  as PU
import qualified Domain.Project.Container               as PC
import Domain.System.Container.Api            (SystemApiContainer(..))
import Control.Applicative                    ((<|>))
import Network.HTTP.Types                     (status404, Method)
import Network.Wai                            ( Application
                                              , pathInfo
                                              , Request
                                              , requestMethod
                                              , Response
                                              , ResponseReceived
                                              , responseLBS
                                              )
import qualified Domain.Central.Responder.Ui.Container as CU

type RouteTree = HashTree Text MethodTree 

type MethodTree = 
  HashTree Method ((Response -> IO ResponseReceived) -> IO ResponseReceived)

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
    pth = pathInfo req <|> [""]
    mth = requestMethod req

notFound :: Application
notFound _ res = res $ responseLBS status404 [] "Not Found" 

rootTree :: RootContainer -> Request -> RouteTree 
rootTree ctn req = emptyT 
  <+> ""    -| only "GET" (index ctn dp req)
  <+> "api" -< apiTree ctn req
  <+> "ui"  -< uiTree  ctn req
  where dp = pack . webDefaultPath . appConfig $ ctn

apiTree :: RootContainer -> Request -> RouteTree
apiTree ctn req = emptyT
  <+> "central" -< centralApiTree ctrCtn
  <+> "project" -< projectApiTree prjCtn req 
  <+> "system"  -< systemApiTree  sysCtn req 
  where
    ctrCtn = centralApiContainer     . central $ ctn
    prjCtn = PC.projectApiContainer' . project $ ctn
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
uiTree ctn req = routes 
  <+> "central"        -< centralUiTree       ctlCtn
  <+> "create-project" -< addProjectUiTree    puiCtn req
  <+> "project"        -< manageProjectUiTree puiCtn req 
  <+> "projects"       -< projectIndexUiTree  puiCtn req
  where
    puiCtn = PC.projectUiContainer' . project $ ctn
    ctlCtn = centralUiContainer . central $ ctn

centralUiTree :: CU.Container -> RouteTree
centralUiTree ct = routes
  <+> "empty" -| only "GET" (CU.emptyView ct)

projectIndexUiTree :: PU.Container -> Request -> RouteTree
projectIndexUiTree ctn _ = emptyT
  <+> "vw"   -| only "GET" (PU.projectIndexVw ctn)
  <+> "list" -| only "GET" (PU.projectList ctn)

addProjectUiTree :: PU.Container  -> Request -> RouteTree
addProjectUiTree ctn req = emptyT
  <+> "vw"     -| only "GET"  (PU.createProjectVw ctn)
  <+> "submit" -| only "POST" (PU.submitProject ctn req)

manageProjectUiTree :: PU.Container -> Request -> RouteTree
manageProjectUiTree ctn req = emptyT
  <+> "vw"    -| only "GET"  (PU.manageProjectVw ctn req)
  <+> "graph" -| only "GET"  (PU.getProjectGraph ctn req)
  <+> "node"  -<
    ( routes
      <+> "panel"       -| only "GET"  (PU.getNodePanel ctn req)
      <+> "edit"        -| only "GET"  (PU.getNodeEdit ctn req)
      <+> "detail"      -| only "GET"  (PU.getNodeDetail ctn req)
      <+> "description" -| only "PUT"  (PU.putNodeDescription ctn req)
      <+> "refresh"     -| only "GET"  (PU.getNodeRefresh ctn req)
      <+> "status"      -| only "PUT"  (PU.putNodeStatus ctn req)
      <+> "title"       -| only "PUT"  (PU.putNodeTitle ctn req)
    )

index :: RootContainer 
  -> Text 
  -> Application 
index = CU.indexView . centralUiContainer . central

