{-# LANGUAGE OverloadedStrings #-}

module Data.HashTree where

import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)

data HashTree k a = Branch (H.HashMap k (HashTree k a)) | Node a

instance (Show a, Show k) => Show (HashTree k a) where
  show (Branch h) = show h
  show (Node x)   = "Node: " ++ show x

(<+>) :: Hashable k => HashTree k a -> k -> HashTree k a -> HashTree k a
t <+> txt = addT t txt

(-<) :: (HashTree k a -> HashTree k a) -> HashTree k a -> HashTree k a
fn -< tr = fn tr

(-|) :: (HashTree k a -> HashTree k a) -> a -> HashTree k a
fn -| x = fn -< Node x

addT :: Hashable k => HashTree k a -> k -> HashTree k a -> HashTree k a  
addT (Branch h1) txt tr = Branch $ H.insert txt tr h1
addT (Node x) _ _       = Node x

emptyT :: HashTree k a
emptyT = Branch H.empty

findPath :: Hashable k => [k] -> HashTree k a -> Maybe a
findPath _ (Node x) = Just x
findPath [] _       = Nothing
findPath (p:ps) (Branch h) = H.lookup p h >>= findPath ps

