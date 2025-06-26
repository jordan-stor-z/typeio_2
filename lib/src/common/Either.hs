{-# LANGUAGE OverloadedStrings #-}
module Common.Either where

import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)
import Data.Text (Text)

listToEither :: a -> [b] -> Either a b
listToEither e []    = Left e
listToEither _ (x:_) = Right x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e Nothing  = Left e
maybeToEither _ (Just x) = Right x

data Tree k a = Branch (H.HashMap k (Tree k a)) | Node a

instance Show k => Show (Tree k a) where
  show (Branch h) = show h
  show (Node _)   = "Node"

(<+>) :: Hashable k => Tree k a -> k -> Tree k a -> Tree k a
t <+> txt = addT t txt

(-<) :: (Tree k a -> Tree k a) -> Tree k a -> Tree k a
fn -< tr = fn tr

(-|) :: (Tree k a -> Tree k a) -> a -> Tree k a
fn -| x = fn -< Node x

addT :: Hashable k => Tree k a -> k -> Tree k a -> Tree k a  
addT (Branch h1) txt tr = Branch $ H.insert txt tr h1
addT (Node x) _ _ = Node x

emptyT :: Tree k a
emptyT = Branch H.empty

findPath :: Hashable k => [k] -> Tree k a -> Maybe a
findPath _ (Node x) = Just x
findPath [] _       = Nothing
findPath (p:ps) (Branch h) = H.lookup p h >>= findPath ps

z :: Tree Text Int
z = emptyT 
  `addT` "something" $ undefined
  `addT` "else" $ undefined
