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
import Data.ByteString.Char8                       (unpack)
import Data.Maybe                                  (listToMaybe)
import qualified Data.Text as T                    (Text, pack, unpack)
import Data.Text.Encoding                          (decodeUtf8)
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

type ValidationError = T.Text 

instance ToJSON LocationResponseHeader where
  toJSON (LocationResponseHeader p t) = 
    object ["path" .= p, "target" .= t]

handleProjectSubmit :: ConnectionPool -> Application 
handleProjectSubmit pl req respond = do
  (pyl, ers) <- runWriter . buildPayload . fst 
    <$> parseRequestBody lbsBackEnd req 
  case ers of
    [] -> do 
      now   <- getCurrentTime
      ins   <- insertProject pyl now pl
      either errorRes success ins
    _  -> formAgain pyl ers
  where
    formAgain pyl es = respond $ responseLBS
      status200
      [("Content-Type", "text/html; charset=utf-8")]
      (renderBS $ projectCreateVwTemplate pyl es)
    errorRes _ = respond $ responseLBS
      status500
      []
      "There was an error in adding the project"
    success _  = respond $ responseLBS
      status200 
      [redirectHeader]
      mempty

redirectHeader :: (HeaderName, ByteString)
redirectHeader = 
  let hd = encode $ LocationResponseHeader
        { path = "/ui/projects/vw"
        , target = "#container"
        }
  in ("Hx-Location", toStrict hd)

hasVal :: (Eq a, Monoid a) => a -> Maybe a
hasVal x = if x == mempty then Nothing else Just x

buildPayload :: [Param] -> Writer [ValidationError] AddProjectPayload
buildPayload params = do
  descr <- getVal "description" params
  ttl   <- getVal "title"       params
  return $ AddProjectPayload (T.pack <$> descr) (T.pack <$> ttl)

getVal :: ByteString -> [Param] -> Writer [ValidationError] (Maybe String)
getVal key params = do
  let vl = hasVal =<< lookup key params
  case vl of
    Nothing -> do
      tell ["Missing parameter in payload: " <> decodeUtf8 key]
      return Nothing
    Just v  -> return $ Just . unpack $ v

insertProject :: 
  AddProjectPayload  
  -> UTCTime 
  -> ConnectionPool 
  -> IO (Either T.Text ())
insertProject pyl tm pl = case pyl of
  AddProjectPayload Nothing _ -> 
    return $ Left "Cannot insert project without a title"
  AddProjectPayload _ Nothing -> 
    return $ Left "Cannot insert project without a description"
  AddProjectPayload (Just descr) (Just ttl) -> flip runSqlPool pl $ do
    stsm <- fmap listToMaybe . select $ do 
      s <- from $ table @M.NodeStatus
      limit 1
      where_ $ s.nodeStatusId ==. val "active"
      pure s
    typm <- fmap listToMaybe . select $ do
      t <- from $ table @M.NodeType
      limit 1
      where_ $ t.nodeTypeId ==. val "project_root"
      pure t
    case (stsm, typm) of
      (Nothing, _) -> return $ Left "No active node status found"
      (_, Nothing) -> return $ Left "No project root node type found"
      (Just status, Just type_) -> do
        pky  <- insert M.Project
        let nd = M.Node
             { M.nodeCreated      = tm
             , M.nodeDeleted      = Nothing
             , M.nodeDescription  = T.unpack descr 
             , M.nodeNodeStatusId = entityKey status 
             , M.nodeNodeTypeId   = entityKey type_ 
             , M.nodeProjectId    = pky 
             , M.nodeTitle        = T.unpack ttl
             , M.nodeUpdated      = tm
             }
        _ <- insert nd
        return $ Right ()

