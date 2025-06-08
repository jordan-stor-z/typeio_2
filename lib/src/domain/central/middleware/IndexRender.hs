{-# LANGUAGE OverloadedStrings #-}

module Domain.Central.Middleware.IndexRender where

import Data.ByteString.Char8 (pack, unpack)
import Data.CaseInsensitive (mk)
import Data.List (intercalate)
import qualified Data.Text as T (pack, unpack)
import Domain.Central.Container.Ui (CentralUiContainer(..))
import Network.HTTP.Types (RequestHeaders)
import Network.Wai (Middleware, requestHeaders, pathInfo)

hasHexHeader :: String -> RequestHeaders -> Bool
hasHexHeader headerName headers = 
  case lookup (mk . pack $ headerName) headers of
    Just value -> unpack value == "true"
    Nothing    -> False

renderIndexMiddleware :: CentralUiContainer -> Middleware
renderIndexMiddleware ct app req respond = 
  let
    path         = pathInfo req
    isHx         = hasHexHeader "HX-Request" hs
    isHxRestore  = hasHexHeader "HX-History-Restore-Request" hs
    isVwPath     = isView path 
    in 
      if isVwPath && not isHx then 
        indexView ct (toPathText path) respond 
      else
      app req respond
  where
    hs  = requestHeaders req
    isView ("ui" : ps) = "vw" `elem` ps 
    isView _           = False
    toPathText = T.pack . ('/' :) . intercalate "/" . map T.unpack

-- VW | HX-Request | Hx-Restore ||| Result
-- F  | T          | T          ||| Skip
-- T  | T          | T          ||| Render Index
