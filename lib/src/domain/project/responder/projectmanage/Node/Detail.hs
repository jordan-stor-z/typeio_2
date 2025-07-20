{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.Node.Detail where

import Domain.Project.Responder.ProjectManage.Link
import Domain.Project.Responder.ProjectManage.Node.Query
import Domain.Project.Responder.ProjectManage.Node.Validation
import Lucid
import Common.Validation               ( (.$)
                                       , isThere 
                                       , isNotEmpty 
                                       , runValidation
                                       , ValidationErr
                                       , valRead
                                       )
import Common.Web.Attributes
import Common.Web.Query                (lookupVal)
import Control.Monad                   (forM_, unless)
import Control.Monad.Reader            (ReaderT)
import Control.Monad.Trans.Class       (lift)
import Control.Monad.Trans.Either      (hoistEither, hoistMaybe, firstEitherT, runEitherT, EitherT)
import Data.Aeson                      ((.=) , object, ToJSON(..))
import Data.Int                        (Int64)
import Data.Text                       (Text, pack, unpack)
import Data.Text.Lazy                  (toStrict)
import Data.Text.Lazy.Builder          (toLazyText)
import Data.Text.Lazy.Builder.Int      (decimal)
import Data.Time                       (UTCTime)
import Data.Time.Format                (defaultTimeLocale, formatTime) 
import Database.Esqueleto.Experimental (from, select, table)
import Database.Persist                (Entity(..))
import Database.Persist.Sql            (ConnectionPool, fromSqlKey, runSqlPool, SqlBackend)
import Network.HTTP.Types              (status200)
import Network.Wai                     (queryString, responseLBS, Request, Response, ResponseReceived)
import Network.HTTP.Types.URI          (QueryText, queryToQueryText)
import qualified Domain.Project.Model as M

class NodeSchema a where
  schemaNodeId :: a -> Int64
  schemaProjectId :: a -> Int64
  schemaTitle :: a -> Text
  schemaDescription :: a -> Text
  schemaUpdated :: a -> UTCTime
  schemaStatusId :: a -> Text
  schemaTypeId :: a -> Text

data NodeDetailError = 
  InvalidParams [ValidationErr]
  | NodeNotFound

data Node = Node
  { nodeId      :: Int64
  , projectId   :: Int64
  , title       :: Text
  , description :: Text
  , updated     :: UTCTime
  , statusId    :: Text
  , typeId      :: Text 
  }

newtype NodeStatus = NodeStatus 
  { nodeStatusId   :: Text
  }

data GetNodeDetailForm = GetNodeDetailForm
  { formProjectId :: Maybe Text 
  , formNodeId    :: Maybe Text 
  }

data GetNodeDetailPayload = GetNodeDetailPayload
  { payloadProjectId :: Int64
  , payloadNodeId    :: Int64
  }

handleErr :: NodeDetailError -> Response
handleErr er = case er of
    (InvalidParams es) ->
      responseLBS
          status200
          [("Content-Type", "text/html")]
        . renderBS . templateInvalidParams 
        $ es
    NodeNotFound -> do 
      responseLBS
          status200
          [("Content-Type", "text/html")]
        . renderBS 
        $ templateNodeNotFound

handleGetNodeEdit :: ConnectionPool
  -> Request 
  -> (Response -> IO ResponseReceived) 
  -> IO ResponseReceived
handleGetNodeEdit pl req respond = do
  rslt <- flip runSqlPool pl . runEitherT $ do
       pyld <- firstEitherT InvalidParams 
                . validateForm 
                $ form
       nde  <- lift (queryNode . payloadNodeId $ pyld)
                >>= hoistMaybe NodeNotFound
                >>= ( firstEitherT InvalidParams
                      . validateNodeProjectId (payloadProjectId pyld) 
                    )
       nsts <- lift queryNodeStatuses
       return (nde, nsts)
  case rslt of
    Left e -> respond $ handleErr e
    Right ( nde
          , nsts
          ) -> respond 
                . responseLBS
                  status200 
                  [("Content-Type", "text-html")] 
                . renderBS 
                . templateNodeEdit nsts
                . toNodeSchema
                $ nde
    where
      form = queryTextToForm 
             . queryToQueryText 
             . queryString 
             $ req

handleGetNodeDetail :: ConnectionPool
  -> Request 
  -> (Response -> IO ResponseReceived) 
  -> IO ResponseReceived
handleGetNodeDetail pl req respond = do
  rslt <- flip runSqlPool pl . runEitherT $ do
       pyld  <- firstEitherT InvalidParams 
            . validateForm 
            $ form
       lift (queryNode . payloadNodeId $ pyld)
          >>= hoistMaybe NodeNotFound
          >>= ( firstEitherT InvalidParams 
                . validateNodeProjectId (payloadProjectId pyld) 
              )
  case rslt of
    Left  e   -> respond . handleErr $ e
    Right nde -> respond 
                 . responseLBS
                   status200 
                   [("Content-Type", "text-html")]
                 . renderBS 
                 . templateNodeDetail 
                 . toNodeSchema
                 $ nde 
    where
      form = queryTextToForm 
             . queryToQueryText 
             . queryString 
             $ req

toNodeSchema :: Entity M.Node -> Node
toNodeSchema (Entity k e) = Node 
  { nodeId      = fromSqlKey k
  , description = pack . M.nodeDescription $ e
  , projectId   = fromSqlKey . M.nodeProjectId $ e
  , statusId    = pack . M.unNodeStatusKey . M.nodeNodeStatusId $ e
  , title       = pack . M.nodeTitle $ e
  , typeId      = pack . M.unNodeTypeKey . M.nodeNodeTypeId $ e
  , updated     = M.nodeUpdated e
  }

queryNodeStatuses :: ReaderT SqlBackend IO [NodeStatus]
queryNodeStatuses = do 
  sts <- select . from $ table @M.NodeStatus
  return . fmap toNodeStatusSchema $ sts
  where
    toNodeStatusSchema (Entity k _) = NodeStatus 
      { nodeStatusId = pack . M.unNodeStatusKey $ k }

queryTextToForm :: QueryText -> GetNodeDetailForm
queryTextToForm qt = GetNodeDetailForm
  { formProjectId = lookupVal "projectId" qt
  , formNodeId    = lookupVal "nodeId" qt
  }

templateNodeNotFound :: Html ()
templateNodeNotFound = do
  div_ [] "Node not found" 

templateInvalidParams :: [ValidationErr] -> Html ()
templateInvalidParams es = do
  div_ []  $ do
    unless (null es) $ do
      div_ [class_ "error-messages"] $ do
        forM_ es $ p_ [class_ "error-message"] . toHtml

templateNodeDetail :: Node -> Html ()
templateNodeDetail nde = do
    header_ [class_ "node-header"] $ do
        h1_ [] (toHtml . title $ nde)
        div_ [id_ "node-actions"] $ do
            button_ [ class_     "pill-button close-button"
                    , hxGet_     (editLink (nodeId nde) (projectId nde))
                    , hxPushUrl_ False 
                    , hxSwap_    "innerHTML"
                    , hxTarget_  "#node-detail"
                    , hxTrigger_ "click"
                    ] $ do
                i_  [class_ "material-icons"] "mode_edit"
            button_ [ class_       "pill-button close-button"
                    , hxGet_     "/ui/central/empty"
                    , hxPushUrl'_ (projectLink (projectId nde))
                    , hxSwap_    "innerHTML"
                    , hxTarget_  "#node-detail"
                    , hxTrigger_ "click"
                    ] $ do
                i_  [class_ "material-icons"] "close"
    section_ [class_ "node-description"] $ do
      p_ [] (toHtml . description $ nde)
    section_ [class_ "node-properties"] $ do
      div_ [class_ "property-item"] $ do
        span_ [class_ "property-label"] "Status:"
        span_ [class_ "property-value"] (toHtml . statusId $ nde)
      div_ [class_ "property-item"] $ do
        span_ [class_ "property-label"] "Type:"
        span_ [class_ "property-value"] (toHtml . showNodeType . typeId $ nde)
      div_ [class_ "property-item"] $ do
        span_ [class_ "property-label"] "Last Updated:"
        span_ [class_ "property-value"] (toHtml . formatUpdated . updated $ nde)
    where
      editLink nid pid = "/ui/project/node/edit" 
                     <> "?nodeId=" 
                     <> (pack . show $ nid)
                     <> "&projectId=" 
                     <> (pack . show $ pid)

templateNodeEdit :: [NodeStatus] -> Node -> Html ()
templateNodeEdit nsts nde = do
    header_ [class_ "node-header"] $ do
        h1_ [] (toHtml . title $ nde)
        div_ [id_ "node-actions"] $ do
            div_ [ class_     "pill-indicator close-button"
                 , id_        "node-edit-indicator"
                 , hxInclude_ "form-node-edit"
                 , hxGet_     $ nodeLink (nodeId nde) (projectId nde)
                 , hxPushUrl_ False 
                 , hxSwap_    "innerHTML"
                 , hxTarget_  "#node-detail"
                 , hxTrigger_ "click"
                 ] empty
            button_ [ class_     "pill-button close-button"
                    , hxGet_     $ nodeLink (nodeId nde) (projectId nde)
                    , hxPushUrl_ False 
                    , hxSwap_    "innerHTML"
                    , hxTarget_  "#node-detail"
                    , hxTrigger_ "click"
                    ] $ do
                i_  [class_ "material-icons"] "close"
    form_ [id_ "form-node-edit"] $ do
      section_ [class_ "column-textarea"] $ do
        label_ [class_ "property-label", for_ "description"] "Description:"
        textarea_ [ name_        "description"
                  , hxPut_       "/ui/project/node/description" 
                  , hxPushUrl_   False
                  , hxInclude_   "this"
                  , hxIndicator_ "#node-edit-indicator"
                  , hxTrigger_   "input changed delay:500ms"
                  , hxVals'_ $ object 
                      [ "projectId" .= (toStrict . toLazyText . decimal $ projectId nde)
                      , "nodeId"    .= (toStrict . toLazyText . decimal $ nodeId nde)
                      ]
                  , hxTarget_ "#node-edit-indicator"
                  ] (toHtml . description $ nde)
      section_ [class_ "node-properties"] $ do
        div_ [class_ "property-item"] $ do
          label_  [for_ "status"] "Status:"
          select_ [ class_    "property-value pill-dropdown",
                    name_     "status",
                    selected_ $ statusId nde,
                    hxPut_    "/ui/project/node/status",
                    hxPushUrl_ False,
                    hxInclude_ "this",
                    hxIndicator_ "#node-edit-indicator",
                    hxTrigger_ "change",
                    hxVals'_ $ object 
                      [ "projectId" .= (toStrict . toLazyText . decimal $ projectId nde)
                      , "nodeId"    .= (toStrict . toLazyText . decimal $ nodeId nde)
                      ],
                    hxTarget_ "#node-edit-indicator"
                   ] $ do
            forM_ nsts $ \nst -> 
              option_ [value_ (nodeStatusId nst)] (toHtml . nodeStatusId $ nst) 
        div_ [class_ "property-item"] $ do
          span_ [class_ "property-label"] "Type:"
          span_ [class_ "property-value"] (toHtml . showNodeType . typeId $ nde)
        div_ [class_ "property-item"] $ do
          span_ [class_ "property-label"] "Last Updated:"
          span_ [class_ "property-value"] (toHtml . formatUpdated . updated $ nde)
    where 
      empty = mempty :: Html ()

formatUpdated :: UTCTime -> Text
formatUpdated = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" 

showNodeType :: Text -> Text
showNodeType typ = case typ of
  "project_root" -> "Root"
  "work"         -> "Work"
  _              -> typ

validateForm :: Monad m 
  => GetNodeDetailForm 
  -> EitherT [ValidationErr] m GetNodeDetailPayload
validateForm fm = hoistEither . runValidation id $ do
  pid <- formProjectId fm 
    .$ unpack
    >>= isThere    "Project id must be present"
    >>= isNotEmpty "Project id must have a value"
    >>= valRead    "Project id must be valid integer"
  nid <- formNodeId fm
    .$ unpack
    >>= isThere    "Node id must be present"
    >>= isNotEmpty "Node id must have a value"
    >>= valRead    "Node id must be valid integer"
  return $ GetNodeDetailPayload <$> pid <*> nid 


