{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.ProjectCreate.Submit where

import Control.Monad.Writer                        (runWriter
                                                   , tell
                                                   , Writer
                                                   )
import Data.Aeson                                  ( (.=)
                                                   , encode
                                                   , object
                                                   , toJSON
                                                   , ToJSON
                                                   )
import Data.ByteString                             (toStrict, ByteString)
import Data.ByteString.Char8                       (pack, unpack)
import Data.Maybe                                  (listToMaybe)
import qualified Data.Text as T                    (Text, pack, unpack)
import Data.Time                                   (getCurrentTime, UTCTime)
import Database.Persist.Sql                        (runSqlPool, entityKey)
import Database.Persist.Postgresql                 (ConnectionPool)
import Database.Esqueleto.Experimental             ( (==.)
                                                   , from
                                                   , insert
                                                   , limit
                                                   , select
                                                   , table
                                                   , val
                                                   , where_
                                                   )
import qualified Domain.Project.Model as M         ( Project(..)
                                                   , Node(..)
                                                   , NodeStatus(..)
                                                   , NodeType(..)
                                                   )
import Domain.Project.Responder.ProjectCreate.View ( AddProjectPayload(..)
                                                   , projectCreateVwTemplate
                                                   )
import Lucid                                       (renderBS)
import Network.HTTP.Types                          (HeaderName, status200, status500)
import Network.Wai                                 (Application, responseLBS)
import Network.Wai.Parse                           (Param, parseRequestBody, lbsBackEnd)

data LocationResponseHeader = LocationResponseHeader
  { path   :: String 
  , target :: String 
  }

type ValidationError = String 

instance ToJSON LocationResponseHeader where
  toJSON (LocationResponseHeader p t) = 
    object ["Path" .= p, "Target" .= t]

handleProjectSubmit :: ConnectionPool -> Application 
handleProjectSubmit pl req respond = do
  prs <-  buildPayload . fst 
    <$> parseRequestBody lbsBackEnd req 
  case prs of
    Left _      -> do
      respond $ responseLBS
        status200
        [("Content-Type", "text/html; charset=utf-8")]
        (renderBS $ projectCreateVwTemplate Nothing)
    Right pyl   -> do
      now   <- getCurrentTime
      inse  <- insertProject pyl now pl
      either errorRes success inse
  where
    errorRes _ = respond $ responseLBS
      status500
      []
      "There was an error in adding the project"
    success _ = respond $ responseLBS
      status200 
      [redirectHeader]
      "There was an error in adding the project"

redirectHeader :: (HeaderName, ByteString)
redirectHeader = 
  let hd = encode $ LocationResponseHeader
        { path = "/ui/projects/vw"
        , target = "#container"
        }
  in ("HX-Location", toStrict hd)

buildPayload :: [Param] -> Either [String] AddProjectPayload
buildPayload params = 
  let result = runWriter $ do
        descr <- getVal "description" params
        ttl   <- getVal "title" params
        return $ AddProjectPayload 
          <$> fmap T.pack descr 
          <*> fmap T.pack ttl
  in case result of
    (Just payload, []) -> Right payload
    (_, errors)        -> Left errors

getVal :: String -> [Param] -> Writer [ValidationError] (Maybe String)
getVal key params = do
  let vl = lookup (pack key) params
  case vl of
    Nothing -> do
      tell ["Missing parameter in payload: " ++ key]
      return Nothing
    Just v  -> return $ Just . unpack $ v

insertProject :: AddProjectPayload 
  -> UTCTime 
  -> ConnectionPool 
  -> IO (Either T.Text ())
insertProject py tm = runSqlPool $ do
  pky  <- insert M.Project
  stsm <- listToMaybe <$> (select $ do 
    s <- from $ table @M.NodeStatus
    limit 1
    where_ $ s.nodeStatusId ==. val "active"
    pure s)
  typm  <- listToMaybe <$> (select $ do
    t <- from $ table @M.NodeType
    limit 1
    where_ $ t.nodeTypeId ==. val "project_root"
    pure t)
  case (stsm, typm) of
    (Nothing, _) -> return $ Left "No active node status found"
    (_, Nothing) -> return $ Left "No project root node type found"
    (Just status, Just type_) -> do
      let nd = M.Node
           { M.nodeAttributes   = mempty
           , M.nodeCreated      = tm
           , M.nodeDeleted      = Nothing
           , M.nodeDescription  = T.unpack . description $ py
           , M.nodeNodeStatusId = entityKey status 
           , M.nodeNodeTypeId   = entityKey type_ 
           , M.nodeProjectId    = pky 
           , M.nodeTitle        = T.unpack . title $ py
           , M.nodeUpdated      = tm
           }
      _ <- insert nd
      return $ Right ()

