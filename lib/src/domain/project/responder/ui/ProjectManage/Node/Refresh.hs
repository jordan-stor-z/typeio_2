{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.Ui.ProjectManage.Node.Refresh where

import Lucid
import Common.Web.Attributes
import Common.Web.Elements
import Common.Validation
import Domain.Project.Responder.Ui.ProjectManage.Node.Query
import Domain.Project.Responder.Ui.ProjectManage.Node.Validation

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
import qualified Data.ByteString.Lazy as B (pack, toStrict, fromStrict)
import Data.Int                   (Int64)
import Data.Text                  (pack, Text, unpack)
import Data.Text.Encoding         (encodeUtf8, decodeUtf8)
import Network.HTTP.Types         (status200, status204, status404, status500, queryToQueryText, QueryText)
import Network.Wai                (Application, responseLBS, Request (queryString))
import Network.Wai.Parse          (parseRequestBody, lbsBackEnd, Param)
import Common.Web.Query (lookupVal)
import Data.Text.Util (intToText)

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
    Left (InvalidParams es) -> rspnd 
                . responseLBS 
                  status500
                  [("Content-Type", "text/html")]
                $ foldr (\e acc -> acc <> (B.fromStrict . encodeUtf8 $ e) <> "\n") mempty es 
    Left MissingNode -> rspnd
                . responseLBS 
                  status404 
                  [("Content-Type", "text/html")]
                $ "Node not found"
    Right (Different, nd) -> 
      rspnd
      . responseLBS 
        status200 
        [("Content-Type", "text/html")]
      . renderBS
      . templateRefresh 
      $ nd
    Right (Same, _) ->
      rspnd
      . responseLBS 
        status204 
        []
      $ mempty 
  where
    form = queryTextToForm 
           . queryToQueryText 
           . queryString 
           $ req

queryTextToForm :: QueryText -> GetNodeRefreshForm 
queryTextToForm qt = GetNodeRefreshForm 
  { formProjectId = lookupVal "projectId" qt
  , formNodeId    = lookupVal "nodeId" qt
  , formClientNodeTitle = lookupVal "clientTitle" qt
  }

templateRefresh :: Entity M.Node -> Html ()
templateRefresh (Entity k e) = do
  g_ [ class_ "hidden" 
     , h_ $ "on load add .flash to "
            <> nsel 
            <> " then wait 500ms"
            <> " then remove .flash from "
            <> nsel 
     ] empty
  toHtml . pack . M.nodeTitle $ e
  where
    empty = mempty :: Html ()
    nsel = (<>) "#node-" . intToText . fromSqlKey $ k

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

