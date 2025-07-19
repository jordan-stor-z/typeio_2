{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Project.Responder.ProjectManage.Node.Save where

import Lucid
import Common.Validation
import Common.Web.Attributes

import qualified Domain.Project.Model as M

import Database.Esqueleto.Experimental
import Control.Monad.Trans.Class  (lift)
import Common.Web.Query           (lookupVal)
import Control.Monad.Reader       (ReaderT)
import Control.Monad.Trans.Either ( hoistEither
                                  , hoistMaybe
                                  , firstEitherT
                                  , runEitherT
                                  , EitherT
                                  )
import Data.Maybe                 (listToMaybe)
import Data.Int                   (Int64)
import Data.Text                  (Text, unpack)
import Data.Text.Encoding         (decodeUtf8)
import Network.HTTP.Types         (status200, status500)
import Network.HTTP.Types.URI     (QueryText, queryToQueryText)
import Network.Wai                (Application, queryString, responseLBS)
import Network.Wai.Parse          (parseRequestBody, lbsBackEnd, Param)

data PutNodeDetailErr =
  InvalidParams [ValidationErr]
  | MissingNode

data PutNodeDescriptionForm = PutNodeDescriptionForm
  { formNodeDescription :: Maybe Text 
  , formNodeId          :: Maybe Text 
  , formProjectId       :: Maybe Text
  }

data PutNodeDescriptionPayload = PutNodeDescriptionPayload
  { payloadDescription :: Text
  , payloadNodeId      :: Int64 
  , payloadProjectId   :: Int64
  }

handlePutDescription :: ConnectionPool -> Application
handlePutDescription pl req rspnd = do
  form <- reqForm . fst <$> parseRequestBody lbsBackEnd req
  rslt <- flip runSqlPool pl . runEitherT $ do
    pyld <- firstEitherT InvalidParams
            . validatePayload
            $ form
    ndM  <- lift 
            . queryNode 
            . payloadNodeId 
            $ pyld
    nd   <- hoistMaybe MissingNode ndM
         >>= ( firstEitherT InvalidParams 
              . validateNodeProjectId pyld
             )
    lift . updateDescription pyld $ nd
  case rslt of
    Left (InvalidParams e) -> rspnd 
                . responseLBS 
                  status500
                  [("Content-Type", "text/html")]
                . renderBS
                . templatePutFail 
                $ e
    Left MissingNode -> rspnd
                . responseLBS 
                  status500
                  [("Content-Type", "text/html")]
                . renderBS
                $ templateNodeNotFound
    Right _ -> rspnd
                . responseLBS 
                  status200 
                  [("Content-Type", "text/html")]
                . renderBS
                $ templatePutSuccess

queryNode :: Int64 
  -> ReaderT SqlBackend IO (Maybe (Entity M.Node))
queryNode nid = do
  ns <-  select $ do
    n <- from $ table @M.Node
    where_ (n.id ==. val nkey)
    limit 1
    pure n
  return . listToMaybe $ ns
  where 
    nkey = toSqlKey @M.Node nid 

updateDescription :: 
  PutNodeDescriptionPayload
  -> Entity M.Node
  -> ReaderT SqlBackend IO ()
updateDescription pyld (Entity k e) = do
  replace k node' 
  where
    node' = e { M.nodeDescription = unpack . payloadDescription $ pyld}

reqForm :: [Param] -> PutNodeDescriptionForm
reqForm ps = PutNodeDescriptionForm
  { formNodeDescription = decodeUtf8 <$> lookup "description" ps
  , formNodeId          = decodeUtf8 <$> lookup "nodeId"      ps 
  , formProjectId       = decodeUtf8 <$> lookup "projectId"   ps 
  }

templatePutSuccess :: Html ()
templatePutSuccess = do
  i_  [class_ "material-icons"] "done"

templateNodeNotFound :: Html ()
templateNodeNotFound = do
  p_ [] "Node not found"

templatePutFail :: [ValidationErr] -> Html ()
templatePutFail es = do
  i_ [class_ "material-icons"] "error"
  ul_ [] $ mapM_ (li_ [] . toHtml) es

validatePayload :: Monad m 
  => PutNodeDescriptionForm  
  -> EitherT [ValidationErr] m PutNodeDescriptionPayload
validatePayload form = 
  hoistEither . runValidation id $ do
    dsc <- formNodeDescription form 
           .$ id 
           >>= isThere    "Node description is required"
           >>= isNotEmpty "Node description cannot be empty"
    nid <- formNodeId form
           .$ unpack
           >>= isThere    "Node id is required"
           >>= isNotEmpty "Node id must have value"
           >>= valRead    "Node id must be valid integer"
    pid <- formProjectId form
           .$ unpack
           >>= isThere    "Project id is required"
           >>= isNotEmpty "Project id must have value"
           >>= valRead    "Project id must be valid integer"
    return $ PutNodeDescriptionPayload 
             <$> dsc 
             <*> nid 
             <*> pid

validateNodeProjectId :: Monad m 
  => PutNodeDescriptionPayload
  -> Entity M.Node 
  -> EitherT [ValidationErr] m (Entity M.Node)
validateNodeProjectId pyld (Entity k e) = hoistEither . runValidation id $ do
  _ <- Just e
    .$ (fromSqlKey . M.nodeProjectId)
    >>= isEq (payloadProjectId pyld) 
          "Invalid state. Node is not part of project"
  return . Just . Entity k $ e

