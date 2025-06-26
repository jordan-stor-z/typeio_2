{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.Api.Node.Post where

import Common.Either                   (listToEither)
import Common.Validation               ( (.$)
                                       , isThere 
                                       , isNotEmpty 
                                       , runValidation
                                       , ValidationErr
                                       , valRead
                                       )
import Control.Monad.Trans.Class       (lift)
import Control.Monad.Trans.Either      (hoistEither, runEitherT)
import Control.Monad.Reader            (ReaderT)
import Data.Aeson                      ( (.=)
                                       , encode
                                       , object
                                       , ToJSON(..)
                                       )
import Data.ByteString                 (ByteString)
import Data.Int                        (Int64)
import Data.Text                       (Text, unpack)
import Data.Text.Encoding              (decodeUtf8)
import Data.Time                       (getCurrentTime, UTCTime)
import Database.Esqueleto.Experimental ( (==.)
                                       , from
                                       , insert
                                       , limit
                                       , select
                                       , table
                                       , toSqlKey
                                       , val
                                       , where_
                                       )
import Database.Persist.Sql            (ConnectionPool, entityKey, runSqlPool, SqlBackend)
import Network.HTTP.Types              (status200, status404, status422, status500)
import Network.Wai                     (Application, responseLBS)
import Network.Wai.Parse               (lbsBackEnd, Param, parseRequestBody)
import qualified Domain.Project.Model  as M

data InsertNodeResult =  
  FailValidation [ValidationErr]
  | MissingStatus 
  | MissingType 
  | ProjectNotFound  

data PostNodeForm = PostNodeForm
  { formDescription :: Maybe ByteString
  , formProjectId   :: Maybe ByteString 
  , formTitle       :: Maybe ByteString 
  }

data PostNodePayload = PostNodePayload
  { description :: Text
  , projectId   :: Int64
  , title       :: Text
  }

instance ToJSON PostNodePayload where
  toJSON (PostNodePayload desc pid ttl) =
    object
      [ "description" .= desc
      , "projectId"   .= pid
      , "title"       .= ttl
      ]

paramToPayload :: [Param] -> PostNodeForm
paramToPayload ps = PostNodeForm 
    { formDescription = lookup "description" ps
    , formProjectId   = lookup "projectId" ps
    , formTitle       = lookup "title" ps
    }

handlePostNode :: ConnectionPool -> Application
handlePostNode pl req respond = do
  form <- paramToPayload . fst <$> parseRequestBody lbsBackEnd req
  res  <- runEitherT $ do
    pyl <- hoistEither $ validateForm form
    now <- lift getCurrentTime
    nde <- lift $ runSqlPool (insertNode pyl now) pl
    hoistEither nde
  case res of
    Right _ -> respond $ responseLBS
      status200 
      [("Content-Type", "application/json")]
      "Ok"
    Left ProjectNotFound     -> notFound ("Project not found" :: Text)
    Left MissingStatus       -> serverExc
    Left MissingType         -> serverExc
    Left (FailValidation es) -> badRequest es
  where
    badRequest es = respond $ responseLBS
      status422 
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= es])
    notFound msg = respond $ responseLBS
      status404 
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= msg])
    serverExc = respond $ responseLBS
      status500 
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= ("Internal server error" :: Text)])

validateForm :: PostNodeForm -> Either InsertNodeResult PostNodePayload
validateForm fm = runValidation FailValidation $ do
    dscr <- formDescription fm 
            .$  decodeUtf8 
            >>= isThere    "Description is required"
            >>= isNotEmpty "Description cannot be empty"
    pid <- formProjectId fm
            .$  decodeUtf8
            >>= isThere    "Project id is required"
            >>= isNotEmpty "Project id cannot be empty"
            >>= valRead    "Project id must be valid integer" 
    ttl <- formTitle fm
            .$  decodeUtf8
             >>= isThere   "Title cannot be empty"
    return $ PostNodePayload <$> dscr <*> pid <*> ttl

insertNode :: PostNodePayload
  -> UTCTime
  -> ReaderT SqlBackend IO (Either InsertNodeResult M.Node) 
insertNode pyl tm = do
  let pid  = projectId pyl
      pkey = toSqlKey @M.Project pid 
      hE a = hoistEither . listToEither a
  runEitherT $ do
    prSel <- lift $ select $ do
      p <- from $ table @M.Project
      where_ $ p.id ==. val pkey
      limit 1 
      pure p
    prj <- hE ProjectNotFound prSel 
    stSel <- lift $ select $ do 
      s <- from $ table @M.NodeStatus
      where_ $ s.nodeStatusId ==. val "active"
      limit 1
      pure s
    sts <- hE MissingStatus stSel
    tpSel <- lift $ select $ do
      t <- from $ table @M.NodeType
      where_ $ t.nodeTypeId ==. val "project_root"
      limit 1
      pure t
    tpe   <- hE MissingType tpSel 
    let nd = M.Node
          { M.nodeCreated      = tm
          , M.nodeDeleted      = Nothing
          , M.nodeDescription  = unpack . description $ pyl 
          , M.nodeNodeStatusId = entityKey sts
          , M.nodeNodeTypeId   = entityKey tpe 
          , M.nodeProjectId    = entityKey prj 
          , M.nodeTitle        = unpack . title $ pyl 
          , M.nodeUpdated      = tm
          }
    _ <- lift $ insert nd
    return nd

