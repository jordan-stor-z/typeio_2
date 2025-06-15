module Common.Either where

listToEither :: a -> [b] -> Either a b
listToEither e []    = Left e
listToEither _ (x:_) = Right x

