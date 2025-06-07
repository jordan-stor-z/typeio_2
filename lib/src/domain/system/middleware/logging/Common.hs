module Domain.System.Middleware.Logging.Common where

import Network.HTTP.Types (RequestHeaders)
import Data.HashMap.Strict (HashMap, fromList)
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 (unpack)
import Data.CaseInsensitive (original)

hashMapHeaders :: RequestHeaders -> HashMap String String
hashMapHeaders = fromList 
  . map (bimap (unpack . original) unpack)
