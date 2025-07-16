{-# LANGUAGE OverloadedStrings #-}

module Domain.Central.Responder.Empty where

import Lucid 
import Network.HTTP.Types (status200)
import Network.Wai        (Response, responseLBS, ResponseReceived)

handleGetEmpty :: (Response -> IO ResponseReceived) -> IO ResponseReceived 
handleGetEmpty res = do 
  res $ responseLBS
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    (renderBS templateEmpty)

templateEmpty :: Html ()
templateEmpty = mempty 
