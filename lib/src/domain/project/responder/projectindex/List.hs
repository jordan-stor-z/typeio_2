{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.ProjectIndex.List where

import Domain.Project.Responder.ProjectManage.Link
import Lucid
import Common.Web.Attributes
import Control.Monad                   (forM_)
import Control.Monad.Reader            (ReaderT)
import Data.Int                        (Int64)
import Data.Time                       (UTCTime)
import Database.Esqueleto.Experimental ( desc
                                       , from
                                       , select
                                       , limit
                                       , offset
                                       , orderBy
                                       , table
                                       , fromSqlKey
                                       )
import Database.Persist.Postgresql     (ConnectionPool)
import Database.Persist.Sql            (runSqlPool, SqlBackend)
import Database.Persist                (Entity(..))
import Domain.Project.Model            (ProjectVw(..))
import Network.HTTP.Types              (status200)
import Network.Wai                     (Response, responseLBS, ResponseReceived)

data ProjectItem = ProjectItem
  { description :: String 
  , projectId   :: Int64 
  , title       :: String
  , lastUpdated :: UTCTime 
  } deriving (Show, Eq)

handleProjectList :: ConnectionPool 
  -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleProjectList cp respond = do
  ps <- runSqlPool queryProjectVw cp
  tp <- case ps of
    [] -> return templateEmptyProjects
    _  -> return . templateList $ ps
  respond $ responseLBS
    status200
    [("Content-Type", "text/html")]
    (renderBS tp)

queryProjectVw :: ReaderT SqlBackend IO [ProjectItem] 
queryProjectVw = do
  fmap (map toSchema') $ select $ do
    p <- from $ table @ProjectVw
    orderBy [desc p.lastUpdated]
    limit 50
    offset 0
    pure p
  where
    toSchema' :: Entity ProjectVw -> ProjectItem
    toSchema' (Entity _ p) = ProjectItem
      { description = projectVwDescription p
      , projectId   = fromSqlKey . projectVwProjectId $ p
      , title       = projectVwTitle p 
      , lastUpdated = projectVwLastUpdated p
      }

templateEmptyProjects :: Html ()
templateEmptyProjects = html_ $ do
  h2_ "Create your first project."

templateList :: [ProjectItem] -> Html ()
templateList ps = div_ [id_ "view"] $ do 
  div_ [id_ "project-index", class_ "card-grid"] $ do
    forM_ ps $ \item -> do
      templateProjectItem item

templateProjectItem :: ProjectItem -> Html ()
templateProjectItem item = do
  div_ [ id_        "project-item"
       , class_     "nav-target"
       , hxGet_     (projectLink . projectId $ item)
       , hxPushUrl_ True
       , hxSwap_    "innerHTML"
       , hxTarget_  "#container"
       , hxTrigger_ "click"
       ] $ do
    span_ [class_ "id"] (display' . projectId $ item)
    div_  [class_ "content"] $ do
      h3_   (toHtml . title $ item)
      span_ (toHtml . description $ item)
      br_ []
      span_ (("Updated: " <>) . display' . lastUpdated $ item)
  where 
    display' :: Show a => a -> Html ()
    display' = toHtml . show
