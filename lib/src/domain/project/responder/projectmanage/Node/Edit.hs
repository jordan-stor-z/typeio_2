{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Project.Responder.ProjectManage.Node.Edit where

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
import Data.Aeson                      ((.=) , object)
import Data.Int                        (Int64)
import Data.Text                       (Text, pack, unpack)
import Data.Text.Util                  (intToText)
import Data.Time                       (UTCTime)
import Data.Time.Format                (defaultTimeLocale, formatTime) 
import Database.Esqueleto.Experimental (from, select, table)
import Database.Persist                (Entity(..))
import Database.Persist.Sql            (ConnectionPool, fromSqlKey, runSqlPool, SqlBackend)
import Network.HTTP.Types              (status200)
import Network.Wai                     (queryString, responseLBS, Request, Response, ResponseReceived)
import Network.HTTP.Types.URI          (QueryText, queryToQueryText)
import qualified Domain.Project.Model as M

data NodeEditErr = 
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

data GetNodeEditForm = GetNodeEditForm 
  { formProjectId :: Maybe Text 
  , formNodeId    :: Maybe Text 
  }

data GetNodeEditPayload = GetNodeEditPayload 
  { payloadProjectId :: Int64
  , payloadNodeId    :: Int64
  }

handleErr :: NodeEditErr -> Response
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
                . templateNodeEdit (fmap toNodeStatusSchema nsts)
                . toNodeSchema
                $ nde
    where
      form = queryTextToForm 
             . queryToQueryText 
             . queryString 
             $ req

formatUpdated :: UTCTime -> Text
formatUpdated = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" 

queryNodeStatuses :: ReaderT SqlBackend IO [Entity M.NodeStatus]
queryNodeStatuses = select . from $ table @M.NodeStatus

queryTextToForm :: QueryText -> GetNodeEditForm 
queryTextToForm qt = GetNodeEditForm 
  { formProjectId = lookupVal "projectId" qt
  , formNodeId    = lookupVal "nodeId" qt
  }

showNodeType :: Text -> Text
showNodeType typ = case typ of
  "project_root" -> "Root"
  "work"         -> "Work"
  _              -> typ

templateNodeNotFound :: Html ()
templateNodeNotFound = do
  div_ [] "Node not found" 

templateInvalidParams :: [ValidationErr] -> Html ()
templateInvalidParams es = do
  div_ []  $ do
    unless (null es) $ do
      div_ [class_ "error-messages"] $ do
        forM_ es $ p_ [class_ "error-message"] . toHtml

templateNodeEdit :: [NodeStatus] -> Node -> Html ()
templateNodeEdit nsts nde = do
  section_ [class_ "column-textarea form-section"] $ do
    label_ [class_ "indicator-label property-label", for_ "title"] $ do
      p_ "Title:"
      div_ [class_ "indicator-box"] empty 
    input_ [ type_        "text"
           , class_       "property-value"
           , id_          "node-title"
           , value_       $ title nde
           , name_        "title"
           , hxPut_       "/ui/project/node/title"
           , hxPushUrl_   False
           , hxInclude_   "this"
           , hxTrigger_   "input changed delay:500ms"
           , hxVals'_ $ object 
               [ "projectId" .= (intToText . projectId $ nde)
               , "nodeId"    .= (intToText . nodeId $ nde)
               ]
           , hxTarget_ "label[for=\"title\"] .indicator-box" 
           , h_ "on input transition <label[for=\"title\"] .indicator-box i /> opacity to 0"
           ]
  section_ [class_ "column-textarea form-section"] $ do
    label_ [class_ "indicator-label property-label", for_ "description"] $ do
      p_ "Description:"
      div_ [class_ "indicator-box"] empty 
    textarea_ [ name_        "description"
              , hxPut_       "/ui/project/node/description" 
              , hxPushUrl_   False
              , hxInclude_   "this"
              , hxTrigger_   "input changed delay:500ms"
              , hxVals'_ $ object 
                  [ "projectId" .= (intToText . projectId $ nde)
                  , "nodeId"    .= (intToText .  nodeId $ nde)
                  ]
              , hxTarget_ "label[for=\"description\"] .indicator-box"
              , h_ "on input transition <label[for=\"description\"] .indicator-box i /> opacity to 0"
              ] (toHtml . description $ nde)
  section_ [id_ "node-properties"] $ do
    article_ [] $ do
      span_ [] $ do
        label_  [for_ "status"] $ p_ "Status:"
        select_ [ class_    "property-value pill-dropdown",
                  name_     "status",
                  selected_ $ statusId nde,
                  hxPut_    "/ui/project/node/status",
                  hxPushUrl_ False,
                  hxInclude_ "this",
                  hxTrigger_ "change",
                  hxVals'_ $ object 
                    [ "projectId" .= (intToText . projectId $ nde)
                    , "nodeId"    .= (intToText . nodeId $ nde)
                    ],
                  hxTarget_ "#status-indicator"
                 ] $ do
          forM_ nsts $ \nst -> 
            option_ [value_ (nodeStatusId nst)] (toHtml . nodeStatusId $ nst) 
      div_ [id_ "status-indicator", class_ "indicator-box"] empty 
    where
      empty = mempty :: Html ()

toNodeStatusSchema :: Entity M.NodeStatus -> NodeStatus
toNodeStatusSchema (Entity k _) = NodeStatus 
  { nodeStatusId = pack . M.unNodeStatusKey $ k }

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

validateForm :: Monad m 
  => GetNodeEditForm 
  -> EitherT [ValidationErr] m GetNodeEditPayload 
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
  return $ GetNodeEditPayload <$> pid <*> nid 

