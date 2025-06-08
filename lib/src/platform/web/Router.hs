{-# LANGUAGE OverloadedStrings #-}

module Platform.Web.Router where

import Common.Web.Types             (Responder)
import Config.App                   (AppConfig(..), webDefaultPath)
import Container.Root               (RootContainer(..))
import Data.Text                    (pack, Text)
import Domain.Central.Container.Api (CentralApiContainer(..))
import Domain.Central.Container.Ui  (CentralUiContainer(..))
import Domain.Project.Container.Api (ProjectApiContainer(..))
import Domain.Project.Container.Ui  (ProjectUiContainer(..))
import Domain.System.Container.Api  (SystemApiContainer(..))
import Network.HTTP.Types           (Method)

index :: RootContainer -> Text -> Responder
index = indexView . centralUiContainer 

appRoutes :: AppConfig -> RootContainer -> Method -> [Text] -> Maybe Responder 
appRoutes cf ct _ []      = Just $ index ct (pack . webDefaultPath $ cf)
appRoutes _ ct mt (p:ps)  = case p of
    "api"   -> api ct mt ps
    "ui"    -> ui  ct mt ps
    _       -> Nothing

projectIndex :: ProjectUiContainer -> Method -> [Text] -> Maybe Responder 
projectIndex ct mt path = case (mt, path) of
    ("GET", ["vw"]) -> Just $ uiProjectIndex ct 
    _               -> Nothing

api :: RootContainer -> Method -> [Text] -> Maybe Responder 
api ct mt pth = case pth of 
    "central" : ps   -> central (centralApiContainer ct) mt ps
    "project" : ps   -> project (projectApiContainer ct) mt ps
    "system"  : ps   -> system  (systemApiContainer ct)  mt ps
    _                -> Nothing

central :: CentralApiContainer -> Method -> [Text] -> Maybe Responder 
central ct mt path = case (mt, path) of
    ("POST", ["seed-database"]) -> Just $ apiSeedDatabase ct
    _                           -> Nothing

project :: ProjectApiContainer -> Method -> [Text] -> Maybe Responder 
project h mt path = case (mt, path) of
    ("GET", ["nodes"])         -> Just $ apiGetNodes h
    ("GET", ["node-types"])    -> Just $ apiGetNodeTypes h 
    ("GET", ["node-statuses"]) -> Just $ apiGetNodeStatuses h 
    ("GET", ["projects"])      -> Just $ apiGetProjects h 
    _                          -> Nothing

system :: SystemApiContainer -> Method -> [Text] -> Maybe Responder 
system ah mt path = case (mt, path) of
    ("GET", ["config"])         -> Just $ apiGetConfig ah
    _                           -> Nothing

ui :: RootContainer -> Method -> [Text] -> Maybe Responder 
ui ct mt pth = case pth of 
    "projects" : ps -> projectIndex (projectUiContainer ct) mt ps
    _               -> Nothing

