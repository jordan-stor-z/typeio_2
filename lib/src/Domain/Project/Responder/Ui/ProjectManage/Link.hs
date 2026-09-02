{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.Ui.ProjectManage.Link where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Util (intToText)

editLink :: Int64 -> Int64 -> Text
editLink nid pid =
  "/ui/project/node/edit"
    <> "?nodeId="
    <> intToText nid
    <> "&projectId="
    <> intToText pid

nodePanelLink :: Int64 -> Int64 -> Text
nodePanelLink nid pid =
  "/ui/project/node/panel"
    <> "?nodeId="
    <> intToText nid
    <> "&projectId="
    <> intToText pid

nodeDetailLink :: Int64 -> Int64 -> Text
nodeDetailLink nid pid =
  "/ui/project/node/detail"
    <> "?nodeId="
    <> intToText nid
    <> "&projectId="
    <> intToText pid

nodeRefreshLink :: Int64 -> Int64 -> Text -> Text
nodeRefreshLink = refreshLink ""

{- | The refresh endpoint re-wraps the node's title, and how many
characters a line may hold depends on the shape the label has to fit:
the D3 path's 45px-radius circle, or the server-computed path's wider
box (#178). Both graphs share this one endpoint, so the same
@layout=server@ flag the graph view itself takes travels on the link
and tells it which. Removed by #181 along with the rest of the flag.
-}
serverNodeRefreshLink :: Int64 -> Int64 -> Text -> Text
serverNodeRefreshLink = refreshLink "&layout=server"

{- | @clientTitle@ goes in unescaped, so it stays last and @extra@ ahead
of it -- appending to the built link would land inside the title.
-}
refreshLink :: Text -> Int64 -> Int64 -> Text -> Text
refreshLink extra nid pid clientTitle =
  "/ui/project/node/refresh?nodeId="
    <> intToText nid
    <> "&projectId="
    <> intToText pid
    <> extra
    <> "&clientTitle="
    <> clientTitle

graphLink :: Int64 -> Text
graphLink pid =
  "/ui/project/graph"
    <> "?projectId="
    <> intToText pid

projectLink :: Int64 -> Text
projectLink = (<>) "/ui/project/vw?projectId=" . intToText
