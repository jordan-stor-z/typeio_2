{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.View where

import Lucid
import Common.Web.Attributes
import Common.Web.Template.MainHeader (templateNavHeader)
import Data.Aeson                     (encode, (.=), object)
import Data.Int                       (Int64)
import Data.Text                      (pack, Text, unpack)
import Network.HTTP.Types             (status200, status403)
import Network.Wai                    (Application, queryString, responseLBS)
import Network.HTTP.Types.URI         (QueryText, queryToQueryText)
import Domain.Project.Responder.ProjectManage.Core (queryProjectId)


handleProjectManageView :: Application
handleProjectManageView req respond = do
  let qt   = queryToQueryText . queryString $ req
      pidE = queryProjectId qt
  case pidE of
    Left er   -> respondBadProjectId (pack . unlines . fmap unpack $ er)
    Right pid -> do
      respond $ responseLBS
        status200 
        [("Content-Type", "text/html")]
        (renderBS $ templateProject pid)
  where
    respondBadProjectId er = respond $ responseLBS
      status403
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= ("Invalid project ID\n" <> er :: Text)])

templateProject :: Int64 -> Html ()
templateProject pid = do
  templateNavHeader "Project"
  let empty     = mempty :: Html ()
      graphLink = "/ui/project/graph" <> "?projectId=" <> (pack . show $ pid) 
  link_ [ rel_ "stylesheet"
        , href_ "/static/styles/views/manage-project.css"
        ]
  script_ [src_ "https://unpkg.com/d3@7"] empty 
  div_ [ id_ "project-view" ] $ do
    div_ [ id_ "tree-container" 
         , hxGet_     graphLink
         , hxPushUrl_ False
         , hxSwap_    "innerHTML"
         , hxTrigger_ "load"
         ] empty
    div_ [ id_ "node-detail" 
         ] empty
