{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.Node.Title where 

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
import Data.Text.Util (intToText)

data PutTitleErr =
  InvalidParams [ValidationErr]
  | MissingNode

data PutNodeTitleForm = PutNodeTitleForm 
  { formNodeTitle  :: Maybe Text 
  , formNodeId     :: Maybe Text 
  , formProjectId  :: Maybe Text
  }

data PutNodeTitlePayload = PutNodeTitlePayload 
  { payloadNodeId    :: Int64 
  , payloadProjectId :: Int64
  , payloadTitle     :: Text 
  }

handlePutTitle :: ConnectionPool -> Application
handlePutTitle pl req rspnd = do
  form <- reqForm . fst <$> parseRequestBody lbsBackEnd req
  rslt <- flip runSqlPool pl . runEitherT $ do
    pyld <- firstEitherT InvalidParams
            . validatePayload
            $ form
    nd   <- lift (queryNode . payloadNodeId $ pyld)
              >>= hoistMaybe MissingNode
              >>= ( firstEitherT InvalidParams 
                   . validateNodeProjectId (payloadProjectId pyld)
                  )
    lift . updateTitle pyld $ nd
    pure (payloadNodeId pyld, payloadTitle pyld)
  case rslt of
    Left (InvalidParams e) -> rspnd 
                . responseLBS 
                  status500
                  [("Content-Type", "text/html")]
                . renderBS
                . templatePostFail 
                $ e
    Left MissingNode -> rspnd
                . responseLBS 
                  status404 
                  [("Content-Type", "text/html")]
                . renderBS
                $ templateNodeNotFound
    Right (nid, ttl) -> rspnd
                . responseLBS 
                  status200 
                  [("Content-Type", "text/html")]
                . renderBS
                . templatePostSuccess ttl
                $ nid

updateTitle :: 
  PutNodeTitlePayload
  -> Entity M.Node
  -> ReaderT SqlBackend IO ()
updateTitle pyld (Entity k e) = do
  replace k node' 
  where
    node' = e { M.nodeTitle = unpack . payloadTitle $ pyld } 

reqForm :: [Param] -> PutNodeTitleForm 
reqForm ps = PutNodeTitleForm 
  { formNodeId          = decodeUtf8 <$> lookup "nodeId"      ps 
  , formProjectId       = decodeUtf8 <$> lookup "projectId"   ps 
  , formNodeTitle       = decodeUtf8 <$> lookup "title"   ps 
  }

templatePostSuccess :: Text -> Int64 -> Html ()
templatePostSuccess ttl nid = do
  i_  [ class_ "material-icons"
      , h_     putNodeTitle 
      ] "done"
  where
    putNodeTitle = "on htmx:beforeCleanupElement from #node-detail put \"" 
                   <> ttl 
                   <> "\" into " 
                   <> nodeTextId 
                   <> " then add .flash to "
                   <> nodeGId
                   <> " then wait 500ms"
                   <> " then remove .flash from "
                   <> nodeGId 
    nodeTextId   = "#node-text-" <> intToText nid
    nodeGId      = "#node-" <> intToText nid

templateNodeNotFound :: Html ()
templateNodeNotFound = do
  p_ [] "Node not found"

templatePostFail :: [ValidationErr] -> Html ()
templatePostFail es = do
  i_ [class_ "material-icons"] "error"
  ul_ [] $ mapM_ (li_ [] . toHtml) es

validatePayload :: Monad m 
  => PutNodeTitleForm  
  -> EitherT [ValidationErr] m PutNodeTitlePayload 
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
    ttl <- formNodeTitle form
           .$ id
           >>= isThere    "Node title is required"
           >>= isNotEmpty "Node title cannot be empty"
    return $ PutNodeTitlePayload 
             <$> nid 
             <*> pid
             <*> ttl

