module Common.Either where

listToEither :: a -> [b] -> Either a b
listToEither e []    = Left e
listToEither _ (x:_) = Right x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e Nothing  = Left e
maybeToEither _ (Just x) = Right x

