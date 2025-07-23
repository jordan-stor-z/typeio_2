module Data.Text.Util where

import Data.Text                  (Text)
import Data.Text.Lazy             (toStrict)
import Data.Text.Lazy.Builder     (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)

intToText :: Integral a => a -> Text
intToText = toStrict . toLazyText . decimal
