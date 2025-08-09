{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.Node.Refresh where

import Lucid
import Common.Web.Attributes
import Common.Validation
import Domain.Project.Responder.ProjectManage.Node.Query
import Domain.Project.Responder.ProjectManage.Node.Validation

import qualified Domain.Project.Model as M

import Database.Esqueleto.Experimental
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Reader       (ReaderT)
import Control.Monad.Trans.Either ( hoistEither
                                  , hoistMaybe
                                  , firstEitherT
                                  , runEitherT
                                  , EitherT
                                  )
import Data.Int                   (Int64)
import Data.Text                  (pack, Text, unpack)
import Data.Text.Encoding         (decodeUtf8)
import Network.HTTP.Types         (status200, status404, status500)
import Network.Wai                (Application, responseLBS)
import Network.Wai.Parse          (parseRequestBody, lbsBackEnd, Param)

data GetNodeRefreshErr =
  InvalidParams [ValidationErr]
  | MissingNode

data NodeRefreshComparisonResult = Same | Different

data GetNodeRefreshForm = GetNodeRefreshForm 
  { formClientNodeTitle :: Maybe Text 
  , formNodeId          :: Maybe Text 
  , formProjectId       :: Maybe Text
  }

data GetNodeRefreshPayload = GetNodeRefreshPayload 
  { payloadNodeId          :: Int64 
  , payloadProjectId       :: Int64
  , payloadClientNodeTitle :: Text 
  }

handleGetNodeRefresh :: ConnectionPool -> Application
handleGetNodeRefresh pl req rspnd = do
  form <- reqForm . fst <$> parseRequestBody lbsBackEnd req
  rslt <- flip runSqlPool pl . runEitherT $ do
    pyld <- firstEitherT InvalidParams
            . validatePayload
            $ form
    nd   <- lift (queryNode . payloadNodeId $ pyld)
            >>=   hoistMaybe MissingNode
            >>= ( firstEitherT InvalidParams 
                . validateNodeProjectId (payloadProjectId pyld)
                )
    let cmpr = if (pack . M.nodeTitle . entityVal $ nd) 
                == payloadClientNodeTitle pyld 
           then Same
           else Different
    pure (cmpr, nd)
  case rslt of
    Left (InvalidParams _) -> rspnd 
                . responseLBS 
                  status500
                  [("Content-Type", "text/html")]
                $ "Invalid parameters"
    Left MissingNode -> rspnd
                . responseLBS 
                  status404 
                  [("Content-Type", "text/html")]
                $ "Node not found"
    Right (Same, nd) -> 
      rspnd
      . responseLBS 
        status200 
        [("Content-Type", "text/html")]
      . renderBS
      . templateYesRefresh
      . entityVal
      $ nd
    Right (Different, nd) ->
      rspnd
      . responseLBS 
        status200 
        [("Content-Type", "text/html")]
      . renderBS
      . templateNoRefresh
      . entityVal
      $ nd

reqForm :: [Param] -> GetNodeRefreshForm 
reqForm ps = GetNodeRefreshForm  
  { formNodeId          = decodeUtf8 <$> lookup "nodeId"      ps 
  , formProjectId       = decodeUtf8 <$> lookup "projectId"   ps 
  , formClientNodeTitle = decodeUtf8 <$> lookup "clientTitle"   ps 
  }

templateYesRefresh :: M.Node -> Html ()
templateYesRefresh nd = do
  span_ [] (pack . M.nodeTitle $ nd)

templateNoRefresh :: M.Node -> Html ()
templateNoRefresh = 
  p_ [] pack . M.nodeTitle 


validatePayload :: Monad m 
  => GetNodeRefreshForm  
  -> EitherT [ValidationErr] m GetNodeRefreshPayload 
validatePayload form = 
  hoistEither . runValidation id $ do
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
    ttl <- formClientNodeTitle form
           .$ id
           >>= isThere    "Node title is required"
           >>= isNotEmpty "Node title cannot be empty"
    return $ GetNodeRefreshPayload 
             <$> nid 
             <*> pid
             <*> ttl

