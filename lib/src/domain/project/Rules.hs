module Domain.Project.Rules where

import Domain.Project.Model 
  ( Node
  , nodeNodeTypeId
  , nodeUpdated
  , unNodeTypeKey
  )
import Data.Time (UTCTime)

isRootNode :: Node -> Bool
isRootNode = (== "project_root") . unNodeTypeKey . nodeNodeTypeId 

getMaxUpdated :: [Node] -> Maybe UTCTime
getMaxUpdated []    = Nothing
getMaxUpdated nodes = Just $ maximum $ map nodeUpdated nodes
