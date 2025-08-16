{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}

module Domain.Project.Responder.Ui.ProjectCreate.Submit where

import Common.Validation                           ( (.$)
                                                   , isThere 
                                                   , isNotEmpty 
                                                   , runValidation
                                                   , ValidationErr
                                                   )
import Control.Monad.Reader                        (ReaderT)
import Control.Monad.Trans.Either                  (hoistEither, hoistMaybe, runEitherT)
import Control.Monad.Trans.Class                   (lift)
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
import Data.Time                                   (getCurrentTime)
import Database.Persist                            (Entity(..))
import Database.Persist.Sql                        ( ConnectionPool
                                                   , runSqlPool
                                                   , SqlBackend
                                                   )
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
import Network.Wai.Parse                           (parseRequestBody, lbsBackEnd, Param)
import qualified Domain.Project.Responder.Ui.ProjectCreate.View as 
  V ( AddProjectForm(..)
    , projectCreateVwTemplate
    )

data ProjectAddResult =
  MissingNodeStatus
  | MissingNodeType
  | FormValidationFail [ValidationErr]

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

paramForm :: [Param] -> V.AddProjectForm
paramForm ps = V.AddProjectForm
  { V.description = decodeUtf8 <$> lookup "description" ps
  , V.title       = decodeUtf8 <$> lookup "title" ps
  }

handleProjectSubmit :: ConnectionPool -> Application
handleProjectSubmit pl req respond = do
  form <- paramForm . fst <$> parseRequestBody lbsBackEnd req
  now  <- getCurrentTime
  rslt <- flip runSqlPool pl . runEitherT $ do
    pyld <- hoistEither . buildPayload $ form
    st   <- lift (queryStatus "active")
             >>= hoistMaybe MissingNodeStatus
    tp   <- lift (queryType "project_root")
             >>= hoistMaybe MissingNodeType
    pkey <- lift . insert $ M.Project
    let nd = M.Node
          { M.nodeCreated      = now
          , M.nodeDeleted      = Nothing
          , M.nodeDescription  = T.unpack . description $ pyld
          , M.nodeNodeStatusId = entityKey st
          , M.nodeNodeTypeId   = entityKey tp
          , M.nodeProjectId    = pkey
          , M.nodeTitle        = T.unpack . title $ pyld
          , M.nodeUpdated      = now
          }
    lift . insert $ nd
  case rslt of
    Left (FormValidationFail es) -> formAgain form es
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

buildPayload :: V.AddProjectForm -> Either ProjectAddResult AddProjectPayload
buildPayload fm = runValidation FormValidationFail $ do
  dscr <- V.description fm 
    .$  id
    >>= isThere    "Description is required"
    >>= isNotEmpty "Description cannot be empty"
  ttl <- V.title fm 
    .$  id
    >>= isThere    "Title is required"
    >>= isNotEmpty "Title cannot be empty"
  return $ AddProjectPayload <$> dscr <*> ttl 

queryStatus :: String
  -> ReaderT SqlBackend IO (Maybe (Entity M.NodeStatus))
queryStatus st = do
  ns <- select $ do
    s <- from $ table @M.NodeStatus
    where_ $ s.nodeStatusId ==. val st
    limit 1
    pure s
  return . listToMaybe $ ns

queryType :: String
  -> ReaderT SqlBackend IO (Maybe (Entity M.NodeType))
queryType tp = do
  ns <- select $ do
    t <- from $ table @M.NodeType
    where_ $ t.nodeTypeId ==. val tp
    limit 1
    pure t
  return . listToMaybe $ ns

