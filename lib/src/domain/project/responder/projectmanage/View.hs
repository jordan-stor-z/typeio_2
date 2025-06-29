{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.View where

import Lucid
import Common.Web.Attributes
import Common.Web.Template.MainHeader (templateNavHeader)
import Data.Aeson                     (encode, (.=), object)
import Data.Maybe                     (listToMaybe)
import Data.Int                       (Int64)
import Data.Text                      (pack, Text, unpack)
import Network.HTTP.Types             (status200, status403)
import Network.Wai                    (Application, pathInfo, responseLBS)
import Text.Read                      (readMaybe)

type ProjectId = Text

lastN :: [a] -> Int -> [a]
lastN [] _ = []
lastN xs count = reverse . take count . reverse $ xs

handleProjectManageView :: Application
handleProjectManageView req respond = do
  let ps = listToMaybe . lastN (pathInfo req) $ 1
      pM = ps >>= (readMaybe . unpack) :: Maybe Int64
  case pM of
    Nothing  -> respondBadProjectId
    Just pid -> do
      respond $ responseLBS
        status200 
        [("Content-Type", "text/html")]
        (renderBS $ templateProject pid)
  where
    respondBadProjectId = respond $ responseLBS
      status403
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= ("Invalid project ID" :: Text)])

templateProject :: Int64 -> Html ()
templateProject pid = do
  templateNavHeader "Project"
  let empty     = mempty :: Html ()
  let graphLink = "/ui/project/graph/" <> (pack . show $ pid) 
  link_ [ rel_ "stylesheet"
        , href_ "/static/styles/views/manage-project.css"
        ]
  script_ [src_ "https://unpkg.com/d3@7"] empty 
  div_    [ id_ "tree-container" 
           , hxGet_     graphLink
           , hxPushUrl_ False
           , hxSwap_    "innerHTML"
           , hxTrigger_ "load"
          ] empty
