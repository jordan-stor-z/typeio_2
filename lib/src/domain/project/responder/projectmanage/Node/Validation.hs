{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.Node.Validation where

import qualified Domain.Project.Model as M
import Common.Validation          ((.$), ValidationErr, runValidation, isEq)
import Control.Monad.Trans.Either (EitherT, hoistEither)
import Data.Int                   (Int64)
import Database.Persist.Sql       (Entity(..), fromSqlKey)

validateNodeProjectId :: Monad m 
  => Int64 
  -> Entity M.Node 
  -> EitherT [ValidationErr] m (Entity M.Node)
validateNodeProjectId pid (Entity k e) = hoistEither . runValidation id $ do
  _ <- Just e
    .$ (fromSqlKey . M.nodeProjectId)
    >>= isEq pid "Invalid state. Node is not part of project"
  return . Just . Entity k $ e

