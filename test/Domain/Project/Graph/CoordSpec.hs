module Domain.Project.Graph.CoordSpec (spec) where

import qualified Data.Map.Strict as M
import Domain.Project.Graph.Coord (assignX)
import Domain.Project.Graph.Layer (Arc (..))
import Domain.Project.Graph.Types (EdgeId (..), NodeId (..))
import Test.Hspec

sep :: Double
sep = 100

nid :: Int -> NodeId
nid = NodeId . fromIntegral

-- | @arc a b@ puts @a@ in the row above @b@.
arc :: Int -> Int -> Int -> Arc
arc i a b = Arc (EdgeId (fromIntegral i)) (nid a) (nid b) False

xOf :: M.Map NodeId Double -> Int -> Double
xOf xs n = M.findWithDefault (-1 / 0) (nid n) xs

-- | Every pair of adjacent centres in a row, in order.
gaps :: M.Map NodeId Double -> [Int] -> [Double]
gaps xs row = zipWith (-) (drop 1 placed) placed
  where
    placed = map (xOf xs) row

spec :: Spec
spec = do
  describe "assignX" $ do
    it "centres a parent between its two children" $ do
      let rows = M.fromList [(0, [nid 1]), (1, [nid 2, nid 3])]
          xs = assignX sep rows [arc 10 1 2, arc 11 1 3]
      xOf xs 1 `shouldBe` (xOf xs 2 + xOf xs 3) / 2

    it "lines a single-child chain up vertically" $ do
      let rows = M.fromList [(0, [nid 1]), (1, [nid 2]), (2, [nid 3])]
          xs = assignX sep rows [arc 10 1 2, arc 11 2 3]
      xOf xs 2 `shouldBe` xOf xs 1
      xOf xs 3 `shouldBe` xOf xs 1

    it "centres a parent over three children" $ do
      let rows = M.fromList [(0, [nid 1]), (1, [nid 2, nid 3, nid 4])]
          xs = assignX sep rows [arc 10 1 2, arc 11 1 3, arc 12 1 4]
      xOf xs 1 `shouldBe` xOf xs 3

    it "keeps every node in a row at least one separation apart" $ do
      let rows = M.fromList [(0, [nid 1]), (1, [nid 2, nid 3, nid 4, nid 5])]
          xs = assignX sep rows [arc 10 1 2, arc 11 1 3, arc 12 1 4, arc 13 1 5]
      all (>= sep) (gaps xs [2, 3, 4, 5]) `shouldBe` True

    it "never reorders a row" $ do
      -- 4 wants to be far left (its only parent is), 2 far right, but
      -- placement may not swap them past each other.
      let rows = M.fromList [(0, [nid 1, nid 9]), (1, [nid 2, nid 3, nid 4])]
          xs = assignX sep rows [arc 10 9 2, arc 11 1 4]
      all (> 0) (gaps xs [2, 3, 4]) `shouldBe` True

    it "keeps rows separated when a wide row sits under a narrow one" $ do
      let rows = M.fromList [(0, [nid 1, nid 2]), (1, [nid 3, nid 4, nid 5])]
          xs = assignX sep rows [arc 10 1 3, arc 11 1 4, arc 12 2 5]
      all (>= sep) (gaps xs [3, 4, 5]) `shouldBe` True
      all (>= sep) (gaps xs [1, 2]) `shouldBe` True

    it "places nodes with no edges without crashing" $ do
      let rows = M.fromList [(0, [nid 1, nid 2])]
          xs = assignX sep rows []
      M.size xs `shouldBe` 2
      all (>= sep) (gaps xs [1, 2]) `shouldBe` True

    it "is deterministic" $ do
      let rows = M.fromList [(0, [nid 1]), (1, [nid 2, nid 3]), (2, [nid 4])]
          as = [arc 10 1 2, arc 11 1 3, arc 12 2 4, arc 13 3 4]
      assignX sep rows as `shouldBe` assignX sep rows as

    it "handles an empty graph" $
      assignX sep M.empty [] `shouldBe` M.empty
