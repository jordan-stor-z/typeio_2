module Domain.Project.Graph.CoordSpec (spec) where

import qualified Data.Map.Strict as M
import Domain.Project.Graph.Coord (assignX)
import Domain.Project.Graph.Layer (Segment (..))
import Domain.Project.Graph.Types
  ( EdgeId (..)
  , EdgeKind (..)
  , LNode (..)
  , NodeId (..)
  , isDummy
  )
import Test.Hspec

{- | Uniform node width, so a "separation" in these tests is just
'width' + 'gap' and the arithmetic stays readable.
-}
width, gap, sep :: Double
width = 60
gap = 40
sep = width + gap

widthOf :: LNode -> Double
widthOf n = if isDummy n then 0 else width

nid :: Int -> LNode
nid = Real . NodeId . fromIntegral

-- | @arc a b@ puts @a@ in the row above @b@.
arc :: Int -> Int -> Int -> Segment
arc i a b = Segment (EdgeId (fromIntegral i)) DependsOn (nid a) (nid b) False

assign :: M.Map Int [LNode] -> [Segment] -> M.Map LNode Double
assign = assignX widthOf gap

xOf :: M.Map LNode Double -> Int -> Double
xOf xs n = M.findWithDefault (-1 / 0) (nid n) xs

-- | Every pair of adjacent centres in a row, in order.
gaps :: M.Map LNode Double -> [Int] -> [Double]
gaps xs row = zipWith (-) (drop 1 placed) placed
  where
    placed = map (xOf xs) row

spec :: Spec
spec = do
  describe "assignX" $ do
    it "centres a parent between its two children" $ do
      let rows = M.fromList [(0, [nid 1]), (1, [nid 2, nid 3])]
          xs = assign rows [arc 10 1 2, arc 11 1 3]
      xOf xs 1 `shouldBe` (xOf xs 2 + xOf xs 3) / 2

    it "lines a single-child chain up vertically" $ do
      let rows = M.fromList [(0, [nid 1]), (1, [nid 2]), (2, [nid 3])]
          xs = assign rows [arc 10 1 2, arc 11 2 3]
      xOf xs 2 `shouldBe` xOf xs 1
      xOf xs 3 `shouldBe` xOf xs 1

    it "centres a parent over three children" $ do
      let rows = M.fromList [(0, [nid 1]), (1, [nid 2, nid 3, nid 4])]
          xs = assign rows [arc 10 1 2, arc 11 1 3, arc 12 1 4]
      xOf xs 1 `shouldBe` xOf xs 3

    it "keeps every node in a row at least one separation apart" $ do
      let rows = M.fromList [(0, [nid 1]), (1, [nid 2, nid 3, nid 4, nid 5])]
          xs = assign rows [arc 10 1 2, arc 11 1 3, arc 12 1 4, arc 13 1 5]
      all (>= sep) (gaps xs [2, 3, 4, 5]) `shouldBe` True

    it "never reorders a row" $ do
      -- 4 wants to be far left (its only parent is), 2 far right, but
      -- placement may not swap them past each other.
      let rows = M.fromList [(0, [nid 1, nid 9]), (1, [nid 2, nid 3, nid 4])]
          xs = assign rows [arc 10 9 2, arc 11 1 4]
      all (> 0) (gaps xs [2, 3, 4]) `shouldBe` True

    it "keeps rows separated when a wide row sits under a narrow one" $ do
      let rows = M.fromList [(0, [nid 1, nid 2]), (1, [nid 3, nid 4, nid 5])]
          xs = assign rows [arc 10 1 3, arc 11 1 4, arc 12 2 5]
      all (>= sep) (gaps xs [3, 4, 5]) `shouldBe` True
      all (>= sep) (gaps xs [1, 2]) `shouldBe` True

    it "places nodes with no edges without crashing" $ do
      let rows = M.fromList [(0, [nid 1, nid 2])]
          xs = assign rows []
      M.size xs `shouldBe` 2
      all (>= sep) (gaps xs [1, 2]) `shouldBe` True

    it "is deterministic" $ do
      let rows = M.fromList [(0, [nid 1]), (1, [nid 2, nid 3]), (2, [nid 4])]
          as = [arc 10 1 2, arc 11 1 3, arc 12 2 4, arc 13 3 4]
      assign rows as `shouldBe` assign rows as

    it "handles an empty graph" $
      assign M.empty [] `shouldBe` M.empty
