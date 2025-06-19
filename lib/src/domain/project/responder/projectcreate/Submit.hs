{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}

module Domain.Project.Responder.ProjectCreate.Submit where

import Common.Either                               (maybeToEither)
import Control.Monad.Trans.Class                   (lift)
import Control.Monad.Writer                        (runWriter
                                                   , tell
                                                   )
import Control.Monad.Trans.Either                  (hoistEither, runEitherT)
import Data.Aeson                                  ( (.=)
                                                   , encode
                                                   , object
                                                   , toJSON
                                                   , ToJSON
                                                   )
import Data.ByteString                             (toStrict, ByteString)
import Data.Maybe                                  (listToMaybe)
import qualified Data.Text as T                    (Text, unpack)
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
import Lucid                                       (renderBS)
import Network.HTTP.Types                          (HeaderName, status200, status500)
import Network.Wai                                 (Application, responseLBS)
import Network.Wai.Parse                           (parseRequestBody, lbsBackEnd)
import qualified Domain.Project.Responder.ProjectCreate.View as 
  V ( AddProjectForm(..)
    , projectCreateVwTemplate
    )

data ProjectAddResult =
  MissingNodeStatus
  | MissingNodeType
  | FormValidationFail [ValidationError]

data LocationResponseHeader = LocationResponseHeader
  { path   :: String 
  , target :: String 
  }

instance ToJSON LocationResponseHeader where
  toJSON (LocationResponseHeader p t) = 
    object ["path" .= p, "target" .= t]

data AddProjectPayload = AddProjectPayload
  { description :: T.Text
  , title       :: T.Text
  }

type ValidationError = T.Text 

handleProjectSubmit :: ConnectionPool -> Application 
handleProjectSubmit pl req respond = do
  ps <- fst <$> parseRequestBody lbsBackEnd req
  let fm = V.AddProjectForm
        { V.description = decodeUtf8 <$> lookup "description" ps 
        , V.title       = decodeUtf8 <$> lookup "title" ps  
        }
  rs <- runEitherT $ do
    p <- hoistEither $ buildPayload fm
    t <- lift getCurrentTime
    n <- lift $ insertProject p t pl
    hoistEither n
  case rs of
    Left (FormValidationFail es) -> formAgain fm es
    Left MissingNodeStatus       -> errorRes ("Missing node status" :: T.Text)
    Left MissingNodeType         -> errorRes ("Missing node type"   :: T.Text)
    Right _                      -> success ()
  where
    formAgain fm es = respond $ responseLBS
      status200
      [("Content-Type", "text/html; charset=utf-8")]
      (renderBS $ V.projectCreateVwTemplate fm es)
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

buildPayload :: V.AddProjectForm 
  -> Either ProjectAddResult AddProjectPayload
buildPayload fm =
  let (pyl, ers) = runWriter $ do
        descr <- do
          case V.description fm of
            Nothing -> tell ["Description is required"]     >> return Nothing
            Just "" -> tell ["Description cannot be empty"] >> return Nothing
            Just d  -> return $ Just d
        ttl <- do
          case V.title fm of
            Nothing -> tell ["Title is required"]     >> return Nothing
            Just "" -> tell ["Title cannot be empty"] >> return Nothing
            Just t  -> return $ Just t
        return $ AddProjectPayload <$> descr <*> ttl 
  in maybeToEither (FormValidationFail ers) pyl
        
insertProject :: 
  AddProjectPayload  
  -> UTCTime 
  -> ConnectionPool 
  -> IO (Either ProjectAddResult ())
insertProject pyl tm pl = 
  flip runSqlPool pl $ do
    let descr = description pyl
        ttl   = title pyl 
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
      (Nothing, _) -> return $ Left MissingNodeStatus 
      (_, Nothing) -> return $ Left MissingNodeType 
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

