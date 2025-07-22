{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.Link where

import Data.Int  (Int64)
import Data.Text (Text, pack)

editLink :: Int64 -> Int64 -> Text
editLink nid pid = "/ui/project/node/edit" 
                     <> "?nodeId=" 
                     <> (pack . show $ nid)
                     <> "&projectId=" 
                     <> (pack . show $ pid)

nodePanelLink :: Int64 -> Int64 -> Text
nodePanelLink nid pid = "/ui/project/node/panel" 
                   <> "?nodeId=" 
                   <> (pack . show $ nid)
                   <> "&projectId=" 
                   <> (pack . show $ pid)

nodeDetailLink :: Int64 -> Int64 -> Text
nodeDetailLink nid pid = "/ui/project/node/detail" 
                     <> "?nodeId=" 
                     <> (pack . show $ nid)
                     <> "&projectId=" 
                     <> (pack . show $ pid)

graphLink :: Int64 -> Text
graphLink pid = "/ui/project/graph" 
                <> "?projectId=" 
                <> (pack . show $ pid)

projectLink :: Int64 -> Text
projectLink = (<>) "/ui/project/vw?projectId=" 
              . pack 
              . show

