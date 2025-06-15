{-# LANGUAGE OverloadedStrings #-}

module Platform.Web.Router where

import Config.App                   (webDefaultPath)
import Container.Root               (RootContainer(..))
import Data.Text                    (pack, Text)
import Domain.Central.Container.Api (CentralApiContainer(..))
import Domain.Central.Container.Ui  (CentralUiContainer(..))
import Domain.Project.Container.Api (ProjectApiContainer(..))
import Domain.Project.Container.Ui  (ProjectUiContainer(..))
import Domain.System.Container.Api  (SystemApiContainer(..))
import Network.HTTP.Types           (Method)
import Network.Wai                  (Request, Response, ResponseReceived)


index :: RootContainer -> Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
index = indexView . centralUiContainer 

appRoutes :: RootContainer -> Request -> Method -> [Text] -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
appRoutes ct _ _ []        = Just $ index ct (pack . webDefaultPath . appConfig $ ct)
appRoutes ct rq mt (p:ps)  = case p of
    "api"   -> api ct mt ps
    "ui"    -> ui  ct rq mt ps
    _       -> Nothing

createProject :: ProjectUiContainer 
  -> Request
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
createProject ct request mt path = case (mt, path) of
    ("GET", ["vw"])      -> Just $ createProjectVw ct 
    ("POST", ["submit"]) -> Just $ submitProject ct request
    _                    -> Nothing

projectIndex :: ProjectUiContainer 
  -> Method
  -> [Text]
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived) 
projectIndex ct mt path = case (mt, path) of
    ("GET", ["vw"])   -> Just $ projectIndexVw ct 
    ("GET", ["list"]) -> Just $ projectList    ct 
    _               -> Nothing

api :: RootContainer 
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
api ct mt pth = case pth of 
    "central" : ps   -> central (centralApiContainer ct) mt ps
    "project" : ps   -> project (projectApiContainer ct) mt ps
    "system"  : ps   -> system  (systemApiContainer ct)  mt ps
    _                -> Nothing

central :: CentralApiContainer 
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
central ct mt path = case (mt, path) of
    ("POST", ["seed-database"]) -> Just $ apiSeedDatabase ct
    _                           -> Nothing

project :: ProjectApiContainer 
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
project h mt path = case (mt, path) of
    ("GET", ["nodes"])         -> Just $ apiGetNodes h
    ("GET", ["node-types"])    -> Just $ apiGetNodeTypes h 
    ("GET", ["node-statuses"]) -> Just $ apiGetNodeStatuses h 
    ("GET", ["projects"])      -> Just $ apiGetProjects h 
    _                          -> Nothing

system :: SystemApiContainer 
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
system ah mt path = case (mt, path) of
    ("GET", ["config"])         -> Just $ apiGetConfig ah
    _                           -> Nothing

ui :: RootContainer 
  -> Request
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
ui ct rq mt pth = case pth of 
    "projects" : ps       -> projectIndex (projectUiContainer ct) mt ps
    "create-project" : ps -> createProject (projectUiContainer ct) rq mt ps
    _                     -> Nothing

