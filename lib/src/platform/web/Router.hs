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
appRoutes ctn _ _ []        = Just 
  . index ctn 
  . pack 
  . webDefaultPath 
  . appConfig $ ctn
appRoutes ctn req mth (p:ps)  = case p of
    "api"   -> api ctn mth ps
    "ui"    -> ui  ctn req mth ps
    _       -> Nothing

createProject :: ProjectUiContainer 
  -> Request
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
createProject ctn req mth pth = case (mth, pth) of
    ("GET",  ["vw"])     -> Just $ createProjectVw ctn
    ("POST", ["submit"]) -> Just $ submitProject ctn req 
    _                    -> Nothing

manageProject :: ProjectUiContainer 
  -> Request
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
manageProject ctn req mth pth = case (mth, pth) of
    ("GET", ["vw", projectId]) -> Just $ manageProjectVw ctn projectId req 
    _               -> Nothing

projectIndex :: ProjectUiContainer 
  -> Method
  -> [Text]
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived) 
projectIndex ctn mth pth = case (mth, pth) of
    ("GET", ["vw"])   -> Just $ projectIndexVw ctn
    ("GET", ["list"]) -> Just $ projectList    ctn 
    _               -> Nothing

api :: RootContainer 
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
api ctn mt pth = case pth of 
    "central" : ps   -> central (centralApiContainer ctn) mt ps
    "project" : ps   -> project (projectApiContainer ctn) mt ps
    "system"  : ps   -> system  (systemApiContainer ctn)  mt ps
    _                -> Nothing

central :: CentralApiContainer 
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
central ctn mth pth = case (mth, pth) of
    ("POST", ["seed-database"]) -> Just $ apiSeedDatabase ctn
    _                           -> Nothing

project :: ProjectApiContainer 
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
project ctn mth pth = case (mth, pth) of
    ("GET", ["nodes"])         -> Just $ apiGetNodes ctn
    ("GET", ["node-types"])    -> Just $ apiGetNodeTypes ctn 
    ("GET", ["node-statuses"]) -> Just $ apiGetNodeStatuses ctn 
    ("GET", ["projects"])      -> Just $ apiGetProjects ctn 
    _                          -> Nothing

system :: SystemApiContainer 
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
system ctn mth pth = case (mth, pth) of
    ("GET", ["config"])         -> Just $ apiGetConfig ctn 
    _                           -> Nothing

ui :: RootContainer 
  -> Request
  -> Method 
  -> [Text] 
  -> Maybe ((Response -> IO ResponseReceived) -> IO ResponseReceived)
ui ctn req mth pth = case pth of 
    "projects"       : ps -> projectIndex (projectUiContainer ctn) mth ps
    "create-project" : ps -> createProject (projectUiContainer ctn) req mth ps
    "project"        : ps -> manageProject (projectUiContainer ctn) req mth ps
    _                     -> Nothing

