{-# LANGUAGE ScopedTypeVariables #-}

module Data.HashTreeSpec (spec) where

import Data.HashTree
import Data.Function (on)
import Data.List (foldl', nubBy)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

-- OverloadedStrings is deliberately NOT on in this file: with it enabled,
-- every bare string literal key (`"a"`, `"api"`, ...) becomes ambiguous
-- (needs both IsString and Hashable resolved from nothing but a string
-- literal), so plain String literals/type annotations are used instead.

spec :: Spec
spec = do
  describe "emptyT / findPath" $ do
    it "finds nothing in an empty tree" $
      findPath ["a"] (emptyT :: HashTree String Int) `shouldBe` Nothing
    it "returns Nothing for an empty path against a Branch" $
      findPath [] (emptyT :: HashTree String Int) `shouldBe` Nothing
    it "returns the leaf even for an empty path, once the tree has already resolved to a Node" $
      findPath ([] :: [String]) (Node (42 :: Int)) `shouldBe` Just 42

  describe "<+> / -| (insert a leaf)" $ do
    it "finds a leaf inserted at the top level" $
      findPath ["a"] ((emptyT :: HashTree String Int) <+> "a" -| 1) `shouldBe` Just 1
    it "returns Nothing for a key that was never inserted" $
      findPath ["missing"] ((emptyT :: HashTree String Int) <+> "a" -| 1) `shouldBe` Nothing
    it "inserting into an already-resolved leaf is a no-op, per addT's Node case" $
      let leaf = Node (1 :: Int) :: HashTree String Int
      in findPath ["anything"] (leaf <+> "a" -| 2) `shouldBe` Just 1

  describe "-< (nest a subtree), mirroring Platform.Web.Router's own usage" $ do
    it "finds a leaf nested two levels deep" $
      -- HashTree is homogeneous (a Branch's children are HashTree k a too,
      -- not HashTree k (HashTree k a)) -- a "subtree" is the same type as
      -- the tree it gets nested into.
      let sub  = (emptyT :: HashTree String String) <+> "central" -| "seed"
          tree = (emptyT :: HashTree String String) <+> "api" -< sub
      in findPath ["api", "central"] tree `shouldBe` Just "seed"
    it "different branches at the same level don't collide with each other" $ do
      let apiTree = (emptyT :: HashTree String String) <+> "nodes" -| "api-nodes"
          uiTree  = (emptyT :: HashTree String String) <+> "nodes" -| "ui-nodes"
          tree    = (emptyT :: HashTree String String)
                      <+> "api" -< apiTree <+> "ui" -< uiTree
      findPath ["api", "nodes"] tree `shouldBe` Just "api-nodes"
      findPath ["ui", "nodes"] tree `shouldBe` Just "ui-nodes"

  describe "findPath's prefix-match behavior" $
    it "stops at the first Node reached, ignoring any leftover path segments" $
      findPath ["a", "extra", "garbage"] ((emptyT :: HashTree String Int) <+> "a" -| 1)
        `shouldBe` Just 1

  prop "findPath resolves every top-level leaf inserted via <+>/-|, for any set of distinct keys" $
    \(kvs :: [(String, Int)]) ->
      let distinct = nubBy ((==) `on` fst) kvs
          tree     = foldl' (\t (k, v) -> t <+> k -| v) (emptyT :: HashTree String Int) distinct
      in all (\(k, v) -> findPath [k] tree == Just v) distinct
