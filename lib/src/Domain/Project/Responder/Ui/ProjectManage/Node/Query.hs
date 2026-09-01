{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Project.Responder.Ui.ProjectManage.Node.Query where

import Control.Monad.Reader (ReaderT)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Database.Esqueleto.Experimental
import qualified Domain.Project.Model as M

queryNode :: Int64 -> ReaderT SqlBackend IO (Maybe (Entity M.Node))
queryNode nid = do
  ns <- select $ do
    n <- from $ table @M.Node
    where_ (n.id ==. val nkey)
    limit 1
    pure n
  return . listToMaybe $ ns
  where
    nkey = toSqlKey @M.Node nid
