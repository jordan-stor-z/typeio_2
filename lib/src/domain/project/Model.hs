{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}  
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Domain.Project.Model where

import Database.Persist.TH 
  ( mkPersist
  , mkMigrate
  , persistLowerCase
  , share
  , sqlSettings
  )
import Data.Time (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project sql=project
  deriving Show

Node sql=node
  created      UTCTime
  deleted      (Maybe UTCTime)
  description  String
  nodeStatusId NodeStatusId constraint=fk_node_nodestatus
  nodeTypeId   NodeTypeId   constraint=fk_node_nodetype 
  projectId    ProjectId    constraint=fk_node_project
  title        String
  updated      UTCTime
  deriving Show

NodeStatus sql=node_status
  nodeStatusId String sql=id
  Primary nodeStatusId
  deriving Show

NodeType sql=node_type
  nodeTypeId String sql=id
  Primary nodeTypeId
  deriving Show

Dependency sql=dependency
  dependencyId Int sql=id
  nodeId       NodeId constraint=fk_dependency_node
  toNodeId     NodeId constraint=fk_dependency_tonode 
  deriving Show
|]

