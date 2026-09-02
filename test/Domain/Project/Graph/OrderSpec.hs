module Domain.Project.Graph.OrderSpec (spec) where

import Data.List (sort)
import qualified Data.Map.Strict as M
import Domain.Project.Graph.Layer (Segment (..))
import Domain.Project.Graph.Order (countCrossings, orderRows)
import Domain.Project.Graph.Types (EdgeId (..), LNode (..), NodeId (..))
import Test.Hspec

nid :: Int -> LNode
nid = Real . NodeId . fromIntegral

-- | @seg i a b@ puts @a@ in the row above @b@.
seg :: Int -> Int -> Int -> Segment
seg i a b = Segment (EdgeId (fromIntegral i)) (nid a) (nid b) False

rowsOf :: [[Int]] -> M.Map Int [LNode]
rowsOf rs = M.fromList (zip [0 ..] (map (map nid) rs))

spec :: Spec
spec = do
  describe "countCrossings" $ do
    it "counts none when edges run in parallel" $ do
      let rows = rowsOf [[1, 2], [3, 4]]
      countCrossings rows [seg 10 1 3, seg 11 2 4] `shouldBe` 0

    it "counts one when two edges swap over" $ do
      let rows = rowsOf [[1, 2], [3, 4]]
      countCrossings rows [seg 10 1 4, seg 11 2 3] `shouldBe` 1

    it "counts every crossing pair when three edges reverse" $ do
      let rows = rowsOf [[1, 2, 3], [4, 5, 6]]
      countCrossings rows [seg 10 1 6, seg 11 2 5, seg 12 3 4] `shouldBe` 3

    it "counts crossings in every gap, not just the first" $ do
      let rows = rowsOf [[1, 2], [3, 4], [5, 6]]
          segments = [seg 10 1 4, seg 11 2 3, seg 12 3 6, seg 13 4 5]
      countCrossings rows segments `shouldBe` 2

    it "counts none for a graph with no edges" $
      countCrossings (rowsOf [[1, 2], [3]]) [] `shouldBe` 0

  describe "orderRows" $ do
    it "untangles a crossing it can fix" $ do
      let rows = rowsOf [[1, 2], [3, 4]]
          segments = [seg 10 1 4, seg 11 2 3]
      countCrossings (orderRows rows segments) segments `shouldBe` 0

    it "never makes an ordering worse" $ do
      -- Already crossing-free: sweeping must not disturb it.
      let rows = rowsOf [[1, 2], [3, 4]]
          segments = [seg 10 1 3, seg 11 2 4]
      countCrossings (orderRows rows segments) segments `shouldBe` 0

    it "reduces crossings on a tangled three-row graph" $ do
      let rows = rowsOf [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
          segments =
            [ seg 10 1 6
            , seg 11 2 5
            , seg 12 3 4
            , seg 13 4 9
            , seg 14 5 8
            , seg 15 6 7
            ]
          before = countCrossings rows segments
          after = countCrossings (orderRows rows segments) segments
      before `shouldSatisfy` (> 0)
      after `shouldSatisfy` (< before)

    it "keeps every row a permutation of what it was given" $ do
      let rows = rowsOf [[1, 2, 3], [4, 5, 6]]
          segments = [seg 10 1 6, seg 11 2 5, seg 12 3 4]
          reordered = orderRows rows segments
      M.map sort reordered `shouldBe` M.map sort rows

    it "leaves a node with no edges in its row" $ do
      let rows = rowsOf [[1, 2], [3, 4, 5]]
          segments = [seg 10 1 4, seg 11 2 3]
          reordered = orderRows rows segments
      fmap sort (M.lookup 1 reordered)
        `shouldBe` Just (sort [nid 3, nid 4, nid 5])

    it "holds a recorded baseline on committed fixtures" $ do
      -- Asserted as exact numbers so that a change which quietly makes
      -- the graph messier fails here rather than going unnoticed.
      --
      -- K(2,2) cannot be untangled: whichever way either row is
      -- ordered, exactly one pair of edges has to cross.
      let k22Rows = rowsOf [[1, 2], [3, 4]]
          k22 = [seg 10 1 3, seg 11 1 4, seg 12 2 3, seg 13 2 4]
      countCrossings (orderRows k22Rows k22) k22 `shouldBe` 1

      -- A three-row graph seeded fully reversed, which the sweeps can
      -- untangle completely.
      let tangledRows = rowsOf [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
          tangled =
            [ seg 20 1 6
            , seg 21 2 5
            , seg 22 3 4
            , seg 23 4 9
            , seg 24 5 8
            , seg 25 6 7
            ]
      countCrossings tangledRows tangled `shouldBe` 6
      countCrossings (orderRows tangledRows tangled) tangled `shouldBe` 0

    it "is deterministic" $ do
      let rows = rowsOf [[1, 2, 3], [4, 5, 6]]
          segments = [seg 10 1 6, seg 11 2 5, seg 12 3 4]
      orderRows rows segments `shouldBe` orderRows rows segments

    it "handles an empty graph" $
      orderRows M.empty [] `shouldBe` M.empty
