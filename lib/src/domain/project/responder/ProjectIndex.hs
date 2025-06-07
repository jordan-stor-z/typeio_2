{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.ProjectIndex where

import Lucid
import Common.Web.Types                    (Responder)
import Control.Monad                       (forM_, when)
import Control.Monad.Writer                (Writer, tell, runWriter)
import Data.Maybe                          (catMaybes, isNothing)
import Data.List                           (find)
import Data.Time                           (UTCTime)
import Database.Esqueleto.Experimental     ( asc
                                           , from
                                           , select
                                           , limit
                                           , offset
                                           , orderBy
                                           , table
                                           , in_
                                           , where_
                                           , valList
                                           )
import Database.Persist.Postgresql         (ConnectionPool)
import Database.Persist.Sql                (runSqlPool)
import Database.Persist                    (Entity(..))
import qualified Domain.Project.Model as M ( Project(..)
                                           , Node(..)
                                           , unProjectKey
                                           )
import Domain.Project.Rules                (getMaxUpdated, isRootNode)
import Data.HashMap.Strict                 (insertWith, toList)
import qualified Data.HashMap.Strict as HM (empty)
import Logging.Core                        (EntryLog, LogLevel(..), runEntryLog)
import Network.HTTP.Types                  (status200)
import Network.Wai                         (responseLBS)

handleProjectIndex :: ConnectionPool -> EntryLog -> Responder
handleProjectIndex cp lg respond = do
  result <- runWriter . projectItems <$> queryProjects cp 
  t <- case result of
    ([], []) -> do
      return templateEmptyProjects
    (ps, []) -> do
      return $ templateIndex ps ""
    (ps, ls) -> do
      runEntryLog lg "Web" Warning (buildErrorMsg ls)
      return $ templateIndex ps "There were errors in the project index" 
  respond $ responseLBS 
    status200 
    [("Content-Type", "text/html")] 
    (renderBS t) 
  where
    buildErrorMsg = ("Project Index Error: " <>) . mconcat

data ProjectItem = ProjectItem
  { description :: String 
  , projectId   :: Int 
  , title       :: String
  , lastUpdated     :: UTCTime 
  } deriving (Show, Eq)

projectItems :: [Entity M.Node] -> Writer [String] [ProjectItem]
projectItems ns = do
  let projectMap = foldr buildMap HM.empty ns
      listGroups = map snd . toList $ projectMap 
  ps <- mapM foldProject listGroups 
  return $ catMaybes ps
  where
    buildMap (Entity _ n) = insertWith (++) (pid n) [n]
    pid = M.unProjectKey . M.nodeProjectId

foldProject :: [M.Node] -> Writer [String] (Maybe ProjectItem)
foldProject ns = do
  let mu = getMaxUpdated ns 
      rn = find isRootNode ns
      pr = setUpdated . projectItem <$> rn <*> mu
  when (isNothing mu) $ tell ["No nodes found for project"]
  when (isNothing rn) $ tell ["No root node found for project"]
  return pr 
  where 
    setUpdated p ma = p { lastUpdated = ma }

projectItem :: M.Node -> ProjectItem
projectItem rt = ProjectItem
  { description = M.nodeDescription rt
  , projectId   = M.unProjectKey . M.nodeProjectId $ rt
  , title       = M.nodeTitle rt
  , lastUpdated = M.nodeUpdated rt
  }

queryProjects :: ConnectionPool -> IO [Entity M.Node]
queryProjects = runSqlPool $ do
  ps <- select $ do
    p <- from $ table @M.Project
    orderBy [asc p.id]
    limit 50
    offset 0
    pure p
  let projectIds = map entityKey ps
  select $ do
    n <- from $ 
      table @M.Node
    where_ $  n.projectId `in_` valList projectIds 
    pure n 

templateEmptyProjects :: Html ()
templateEmptyProjects = html_ $ do
  h1_ "Project Index"
  div_ [id_ "project-index"] $ do
    p_ "Create your first project."

templateIndex :: [ProjectItem] -> String -> Html ()
templateIndex ps er = div_ [class_ "view"] $ do 
  h1_ "Project Index"
  div_ [id_ "project-index"] $ do
    forM_ ps $ \item -> do
      templateProjectItem item
  if null er
  then return ()
  else div_ [id_ "error"] $ do
    h2_ "Error"
    p_ (toHtml er)

templateProjectItem :: ProjectItem -> Html ()
templateProjectItem item = do
  div_ [id_ "project-item"] $ do
    h2_ (toHtml . title $ item)
    p_ (toHtml . description $ item)
    p_ ("Last updated: " <> toHtml (show $ lastUpdated item))
