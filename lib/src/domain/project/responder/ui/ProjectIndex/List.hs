{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.Ui.ProjectIndex.List where

import Domain.Project.Responder.Ui.ProjectManage.Link
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
import Network.HTTP.Types              (status200)
import Network.Wai                     (Response, responseLBS, ResponseReceived)
import qualified Domain.Project.Model as M 
import Data.Text.Util (intToText)

handleProjectList :: ConnectionPool 
  -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleProjectList cp respond = do
  ps <- runSqlPool queryProjectVw cp
  tp <- case ps of
    [] -> pure templateEmptyProjects
    _  -> pure . templateList . map entityVal $ ps
  respond $ responseLBS
    status200
    [("Content-Type", "text/html")]
    (renderBS tp)

queryProjectVw :: ReaderT SqlBackend IO [Entity M.ProjectVw]
queryProjectVw = 
  select $ do
    p <- from $ table @M.ProjectVw
    orderBy [desc p.lastUpdated]
    limit 50
    offset 0
    pure p

templateEmptyProjects :: Html ()
templateEmptyProjects = html_ $ do
  h2_ "Create your first project."

templateList :: [M.ProjectVw] -> Html ()
templateList ps = div_ [id_ "view"] $ do 
  div_ [id_ "project-index", class_ "card-grid"] $ do
    forM_ ps $ \p -> do
      div_ [ id_        "project-item"
           , class_     "nav-target"
           , hxGet_     (projectLink . fromSqlKey . M.projectVwProjectId $ p)
           , hxPushUrl_ True
           , hxSwap_    "innerHTML"
           , hxTarget_  "#container"
           , hxTrigger_ "click"
           ] $ do
        span_ [class_ "id"] $ 
          toHtml 
          . intToText 
          . fromSqlKey 
          . M.projectVwProjectId 
          $ p
        div_  [class_ "content"] $ do
          h3_   (toHtml . M.projectVwTitle $ p)
          span_ (toHtml . M.projectVwDescription $ p)
          br_ []
          span_ $ 
            ("Updated: " <>) 
            . toHtml 
            . show 
            . M.projectVwLastUpdated 
            $ p
