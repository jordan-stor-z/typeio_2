{-# LANGUAGE OverloadedStrings #-}
module Common.Either where

import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import Network.Wai (Application)
import Clay (a)

listToEither :: a -> [b] -> Either a b
listToEither e []    = Left e
listToEither _ (x:_) = Right x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e Nothing  = Left e
maybeToEither _ (Just x) = Right x

data Tree a = Branch (H.HashMap Text (Tree a)) | Node a

instance Show (Tree a) where
  show (Branch h) = show h
  show (Node _) = "Node"

(<+>) :: Tree a -> Text -> Tree a -> Tree a
t <+> txt = addT t txt

(-<) :: (Tree a -> Tree a) -> Tree a -> Tree a
fn -< tr = fn tr

(-|) :: (Tree a -> Tree a) -> a -> Tree a
fn -| x = fn -< Node x

addT :: Tree a -> Text -> Tree a -> Tree a  
addT (Branch h1) txt tr = Branch $ H.insert txt tr h1
addT (Node x) _ _ = Node x

emptyT :: Tree a
emptyT = Branch H.empty

findPath :: [Text] -> Tree a -> Maybe a
findPath _ (Node x) = Just x
findPath [] _       = Nothing
findPath (p:ps) (Branch h) = H.lookup p h >>= findPath ps

z :: Tree Int
z = emptyT 
  `addT` "something" $ undefined
  `addT` "else" $ undefined
