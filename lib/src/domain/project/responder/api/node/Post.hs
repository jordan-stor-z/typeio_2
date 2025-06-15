{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.Api.Node.Post where

import Common.Either                   (listToEither)
import Control.Monad.Trans.Class       (lift)
import Control.Monad.Trans.Either      (hoistEither, runEitherT)
import Control.Monad.Writer            (runWriter, tell, Writer)
import Data.Aeson                      ( (.=)
                                       , encode
                                       , object
                                       , ToJSON(..)
                                       )
import Data.ByteString                 (ByteString)
import Data.Int                        (Int64)
import Data.Text                       (pack, Text, unpack)
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
import Database.Persist.Sql            (ConnectionPool, entityKey, runSqlPool)
import Network.HTTP.Types              (status200, status404, status422, status500)
import Network.Wai                     ( Application, responseLBS)
import Network.Wai.Parse               (parseRequestBody, Param, lbsBackEnd)
import Text.Read                       (readMaybe)

import qualified Data.ByteString.Char8 as B (unpack)
import qualified Domain.Project.Model  as M

type PostNodeError = Text

data InsertNodeResult =  
  FailValidation 
  | MissingStatus 
  | MissingType 
  | ProjectNotFound  

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

handlePostNode' :: ConnectionPool -> Application
handlePostNode' pl req respond = do
  ps  <- parseRequestBody lbsBackEnd req
  res <- runEitherT $ do
    pyl <- hoistEither $ buildPayload . fst $ ps
    now <- lift getCurrentTime
    nde <- lift $ insertNode pyl now pl
    hoistEither nde
  case res of
    Right _ -> respond $ responseLBS
      status200 
      [("Content-Type", "application/json")]
      "Ok"
    Left (ProjectNotFound, _) -> notFound ("Project not found" :: Text)
    Left (MissingStatus,   _) -> serverExc
    Left (MissingType,     _) -> serverExc
    Left (FailValidation, es) -> badRequest es
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

buildPayload :: [Param] -> Either (InsertNodeResult, [PostNodeError]) PostNodePayload
buildPayload ps =
  let (pyl, errs) = runWriter $ do
        desc <- getVal "description" ps
        pid  <- getVal "projectId" ps >>= readProjectId
        ttl  <- getVal "title" ps
        return $ PostNodePayload 
          <$> fmap pack desc 
          <*> pid 
          <*> fmap pack ttl
  in case pyl of
    Nothing -> Left (FailValidation, errs)
    Just p  -> Right p

insertNode :: PostNodePayload
  -> UTCTime
  -> ConnectionPool
  -> IO (Either (InsertNodeResult, [PostNodeError]) M.Node)
insertNode pyl tm = runSqlPool $ do
  let pid  = projectId pyl
  let pkey = toSqlKey @M.Project pid 
  runEitherT $ do
    prSel <- lift $ select $ do
      p <- from $ table @M.Project
      where_ $ p.id ==. val pkey
      limit 1 
      pure p
    prj <- hoistEither 
           . (listToEither . projectNotFoundRes $ pid) 
           $ prSel 
    stSel <- lift $ select $ do 
      s <- from $ table @M.NodeStatus
      where_ $ s.nodeStatusId ==. val "active"
      limit 1
      pure s
    sts <- hoistEither
           . listToEither statusNotFoundRes 
           $ stSel 
    tpSel <- lift $ select $ do
      t <- from $ table @M.NodeType
      where_ $ t.nodeTypeId ==. val "project_root"
      limit 1
      pure t
    tpe   <- hoistEither
           . listToEither typeNotFoundRes 
           $ tpSel 
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
  where
    projectNotFoundRes p =
      (ProjectNotFound, ["Project with id " <> pack (show p) <> " not found"])
    statusNotFoundRes =
      (MissingStatus, ["No \"active\" node status found"])
    typeNotFoundRes =
      (MissingType, ["No \"project_root\" node type found"])



getVal :: ByteString -> [Param] -> Writer [PostNodeError] (Maybe String)
getVal key params = do
  let vl = hasVal =<< lookup key params
  case vl of
    Nothing -> do
      tell ["Missing parameter in payload: " <> decodeUtf8 key]
      return Nothing
    Just v  -> return $ Just . B.unpack $ v

hasVal :: (Eq a, Monoid a) => a -> Maybe a
hasVal x = if x == mempty then Nothing else Just x

readProjectId :: Maybe String -> Writer [PostNodeError] (Maybe Int64)
readProjectId Nothing = return Nothing
readProjectId (Just pidStr) = 
  case readMaybe pidStr :: Maybe Int64 of
    Nothing -> do
      tell ["Invalid project ID: " <> pack pidStr]
      return Nothing
    Just pid -> return (Just pid)
