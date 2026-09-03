{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Project.Responder.Api.Node.Post where

import Common.Validation
  ( ValidationErr
  , isNotEmpty
  , isThere
  , runValidation
  , valRead
  , (.$)
  )
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (hoistEither, hoistMaybe, runEitherT)
import Data.Aeson
  ( ToJSON (..)
  , encode
  , object
  , (.=)
  )
import Data.ByteString (ByteString)
import Data.Either (listToEither)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Database.Esqueleto.Experimental
  ( Entity
  , from
  , insert
  , limit
  , select
  , table
  , toSqlKey
  , val
  , where_
  , (==.)
  )
import Database.Persist (Entity (..))
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import qualified Domain.Project.Model as M
import Network.HTTP.Types (status200, status404, status422, status500)
import Network.Wai (Application, responseLBS)
import Network.Wai.Parse (Param, lbsBackEnd, parseRequestBody)

data InsertNodeResult
  = FailValidation [ValidationErr]
  | MissingStatus
  | MissingType
  | ProjectNotFound

data PostNodeForm = PostNodeForm
  { formDescription :: Maybe ByteString
  , formProjectId :: Maybe ByteString
  , formTitle :: Maybe ByteString
  }

data PostNodePayload = PostNodePayload
  { description :: Text
  , projectId :: Int64
  , title :: Text
  }

instance ToJSON PostNodePayload where
  toJSON (PostNodePayload desc pid ttl) =
    object
      [ "description" .= desc
      , "projectId" .= pid
      , "title" .= ttl
      ]

paramToPayload :: [Param] -> PostNodeForm
paramToPayload ps =
  PostNodeForm
    { formDescription = lookup "description" ps
    , formProjectId = lookup "projectId" ps
    , formTitle = lookup "title" ps
    }

handlePostNode :: ConnectionPool -> Application
handlePostNode pl req respond = do
  form <- paramToPayload . fst <$> parseRequestBody lbsBackEnd req
  now <- getCurrentTime
  rslt <- flip runSqlPool pl . runEitherT $ do
    pyl <- hoistEither . validateForm $ form
    pr <-
      lift (queryProject . projectId $ pyl)
        >>= hoistMaybe ProjectNotFound
    st <-
      lift (queryStatus "active")
        >>= hoistMaybe MissingStatus
    tp <-
      lift (queryType "work")
        >>= hoistMaybe MissingType
    let nd = toNode now pyl pr st tp
    ky <- lift . insert $ nd
    -- No @project.dependency@ row is written here any more (#198).
    --
    -- One used to be, pointing the new node at the project root, to say
    -- "this node belongs to this project". But @node.project_id@ — set
    -- on the row just inserted — already records exactly that, so the
    -- edge was duplicate data in a table that means something else. The
    -- graph read it as a real dependency, correctly drew the dependent
    -- above what it waits on, and so put the project root underneath
    -- every node in the project.
    --
    -- The graph now derives containment from @project_id@ instead, and
    -- migration 000009 removes the rows this used to write. A row in
    -- @project.dependency@ means a genuine ordering between two pieces
    -- of work, and nothing else.
    pure $ Entity ky nd
  case rslt of
    Right _ ->
      respond $
        responseLBS
          status200
          [("Content-Type", "application/json")]
          "Ok"
    Left ProjectNotFound -> notFound ("Project not found" :: Text)
    Left MissingStatus -> serverExc
    Left MissingType -> serverExc
    Left (FailValidation es) -> badRequest es
  where
    badRequest es =
      respond $
        responseLBS
          status422
          [("Content-Type", "application/json")]
          (encode $ object ["error" .= es])
    notFound msg =
      respond $
        responseLBS
          status404
          [("Content-Type", "application/json")]
          (encode $ object ["error" .= msg])
    serverExc =
      respond $
        responseLBS
          status500
          [("Content-Type", "application/json")]
          (encode $ object ["error" .= ("Internal server error" :: Text)])
    toNode now pyl pr st tp =
      M.Node
        { M.nodeCreated = now
        , M.nodeDeleted = Nothing
        , M.nodeDescription = unpack . description $ pyl
        , M.nodeNodeStatusId = entityKey st
        , M.nodeNodeTypeId = entityKey tp
        , M.nodeProjectId = entityKey pr
        , M.nodeTitle = unpack . title $ pyl
        , M.nodeUpdated = now
        }

validateForm :: PostNodeForm -> Either InsertNodeResult PostNodePayload
validateForm fm = runValidation FailValidation $ do
  dscr <-
    formDescription fm
      .$ decodeUtf8
      >>= isThere "Description is required"
      >>= isNotEmpty "Description cannot be empty"
  pid <-
    formProjectId fm
      .$ (unpack . decodeUtf8)
      >>= isThere "Project id is required"
      >>= isNotEmpty "Project id cannot be empty"
      >>= valRead "Project id must be valid integer"
  ttl <-
    formTitle fm
      .$ decodeUtf8
      >>= isThere "Title cannot be empty"
  return $ PostNodePayload <$> dscr <*> pid <*> ttl

queryProject :: Int64 -> ReaderT SqlBackend IO (Maybe (Entity M.Project))
queryProject pid = do
  prj <- select $ do
    p <- from $ table @M.Project
    where_ $ p.id ==. (val . toSqlKey @M.Project $ pid)
    limit 1
    pure p
  return . listToMaybe $ prj

queryStatus :: Text -> ReaderT SqlBackend IO (Maybe (Entity M.NodeStatus))
queryStatus st = do
  ns <- select $ do
    s <- from $ table @M.NodeStatus
    where_ $ s.nodeStatusId ==. (val . unpack $ st)
    limit 1
    pure s
  return . listToMaybe $ ns

queryType :: Text -> ReaderT SqlBackend IO (Maybe (Entity M.NodeType))
queryType tpe = do
  tp <- select $ do
    t <- from $ table @M.NodeType
    where_ $ t.nodeTypeId ==. (val . unpack $ tpe)
    limit 1
    pure t
  return . listToMaybe $ tp

insertNode ::
  PostNodePayload ->
  UTCTime ->
  ReaderT SqlBackend IO (Either InsertNodeResult M.Node)
insertNode pyl tm = do
  let pid = projectId pyl
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
    tpe <- hE MissingType tpSel
    let nd =
          M.Node
            { M.nodeCreated = tm
            , M.nodeDeleted = Nothing
            , M.nodeDescription = unpack . description $ pyl
            , M.nodeNodeStatusId = entityKey sts
            , M.nodeNodeTypeId = entityKey tpe
            , M.nodeProjectId = entityKey prj
            , M.nodeTitle = unpack . title $ pyl
            , M.nodeUpdated = tm
            }
    _ <- lift $ insert nd
    return nd
