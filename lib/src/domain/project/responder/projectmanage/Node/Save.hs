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
import Data.Maybe                   (listToMaybe)
import Data.Int                     (Int64)
import Data.Text                    (Text, unpack)
import Network.HTTP.Types           (status200, status500)
import Network.HTTP.Types.URI       (QueryText, queryToQueryText)
import Network.Wai                  (Application, queryString, responseLBS)

data PostNodeDetailErr =
  InvalidParams [ValidationErr]
  | MissingNode

data PostNodeDescriptionForm = PostNodeDescriptionForm
  { formNodeDescription :: Maybe Text 
  , formNodeId          :: Maybe Text 
  , formProjectId       :: Maybe Text
  }

data PostNodeDescriptionPayload = PostNodeDescriptionPayload
  { payloadDescription :: Text
  , payloadNodeId      :: Int64 
  , payloadProjectId   :: Int64
  }

handlePostDescription :: ConnectionPool -> Application
handlePostDescription pl req rspnd = do
  rslt <- flip runSqlPool pl . runEitherT $ do
    pyld <- firstEitherT InvalidParams
            . validatePostDescriptionPayload
            $ form
    ndM  <- lift 
            . queryNode 
            . payloadNodeId 
            $ pyld
    nd   <- hoistMaybe MissingNode ndM
              >>= ( firstEitherT InvalidParams 
                    . validateNodeProjectId pyld
                  )
    _    <- lift 
            . updateNodeDescription pyld 
            $ nd
    pure nd 
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
                  status500
                  [("Content-Type", "text/html")]
                . renderBS
                $ templateNodeNotFound
    Right nd -> rspnd
                . responseLBS 
                  status200 
                  [("Content-Type", "text/html")]
                . renderBS
                $ templatePostSuccess
  where
    form = queryTextToForm
           . queryToQueryText
           . queryString
           $ req

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

updateNodeDescription :: 
  PostNodeDescriptionPayload
  -> Entity M.Node
  -> ReaderT SqlBackend IO ()
updateNodeDescription pyld (Entity k e) = do
  replace k node' 
  where
    node' = e { M.nodeDescription = unpack . payloadDescription $ pyld}

queryTextToForm :: QueryText -> PostNodeDescriptionForm
queryTextToForm qt = PostNodeDescriptionForm
  { formNodeDescription = lookupVal "description" qt
  , formNodeId          = lookupVal "nodeId"      qt
  , formProjectId       = lookupVal "projectId"   qt
  }

templatePostSuccess :: Html ()
templatePostSuccess = do
  i_  [class_ "material-icons"] "done"

templateNodeNotFound :: Html ()
templateNodeNotFound = do
  p_ [] "Node not found"

templatePostFail :: [ValidationErr] -> Html ()
templatePostFail es = do
  i_ [class_ "material-icons"] "error"
  ul_ [] $ mapM_ (li_ [] . toHtml) es

validatePostDescriptionPayload :: Monad m 
  => PostNodeDescriptionForm  
  -> EitherT [ValidationErr] m PostNodeDescriptionPayload
validatePostDescriptionPayload form = 
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
    return $ PostNodeDescriptionPayload 
             <$> dsc 
             <*> nid 
             <*> pid

validateNodeProjectId :: Monad m 
  => PostNodeDescriptionPayload
  -> Entity M.Node 
  -> EitherT [ValidationErr] m (Entity M.Node)
validateNodeProjectId pyld (Entity k e) = hoistEither . runValidation id $ do
  _ <- Just e
    .$ (fromSqlKey . M.nodeProjectId)
    >>= isEq (payloadProjectId pyld) 
          "Invalid state. Node is not part of project"
  return . Just . Entity k $ e
