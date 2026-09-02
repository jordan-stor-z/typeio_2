module Domain.Project.Graph.LayoutSpec (spec) where

import Data.List (find)
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Text as T
import Domain.Project.Graph.Layout (layout)
import Domain.Project.Graph.Types
import Test.Hspec

cfg :: LayoutConfig
cfg = defaultLayoutConfig

node :: Int -> LayoutNode
node n = LayoutNode (NodeId (fromIntegral n)) WorkNode (T.pack ("node " <> show n))

rootNode :: Int -> LayoutNode
rootNode n = (node n) {lnKind = RootNode}

-- | @dep i a b@: @b@ depends on @a@, so @a@ must finish first.
dep :: Int -> Int -> Int -> LayoutEdge
dep i a b =
  LayoutEdge
    { leId = EdgeId (fromIntegral i)
    , leDependency = NodeId (fromIntegral a)
    , leDependent = NodeId (fromIntegral b)
    }

placed :: Diagram -> Int -> PlacedNode
placed d n =
  fromJust (find ((== NodeId (fromIntegral n)) . pnId) (diagramNodes d))

topOf :: PlacedNode -> Double
topOf = ptY . pnTopLeft

centreX :: PlacedNode -> Double
centreX p = ptX (pnTopLeft p) + szW (pnSize p) / 2

-- | Do two placed boxes overlap in both axes?
overlaps :: PlacedNode -> PlacedNode -> Bool
overlaps a b = ov ptX szW && ov ptY szH
  where
    ov coord dim =
      coord (pnTopLeft a) < coord (pnTopLeft b) + dim (pnSize b)
        && coord (pnTopLeft b) < coord (pnTopLeft a) + dim (pnSize a)

pairs :: [a] -> [(a, a)]
pairs xs = [(a, b) | (i, a) <- zip [0 :: Int ..] xs, (j, b) <- zip [0 ..] xs, i < j]

spec :: Spec
spec = do
  describe "layout" $ do
    it "places every node exactly once" $ do
      let d = layout cfg [node 1, node 2, node 3] [dep 10 2 1]
      map pnId (diagramNodes d)
        `shouldBe` [NodeId 1, NodeId 2, NodeId 3]

    it "never overlaps two node boxes" $ do
      let ns = map node [1 .. 6]
          es = [dep 10 2 1, dep 11 3 1, dep 12 4 2, dep 13 5 2, dep 14 6 3]
          d = layout cfg ns es
      filter (uncurry overlaps) (pairs (diagramNodes d)) `shouldBe` []

    it "draws a dependency below the node that depends on it" $ do
      let d = layout cfg [node 1, node 2] [dep 10 2 1]
      topOf (placed d 2) `shouldSatisfy` (> topOf (placed d 1))

    it "puts the project root in the top row" $ do
      let d = layout cfg [rootNode 1, node 2, node 3] [dep 10 2 1, dep 11 3 2]
          tops = map topOf (diagramNodes d)
      topOf (placed d 1) `shouldBe` minimum tops

    it "ends each edge on the dependent, which is where the arrowhead goes" $ do
      let d = layout cfg [node 1, node 2] [dep 10 2 1]
          e = head (diagramEdges d)
          lastPoint = last (pePoints e)
          dependentBox = placed d 1
      -- The final point lies on the dependent's box, not the dependency's.
      ptY lastPoint
        `shouldSatisfy` \y ->
          y >= topOf dependentBox
            && y <= topOf dependentBox + szH (pnSize dependentBox)

    it "gives every edge a polyline" $ do
      let d = layout cfg [node 1, node 2, node 3] [dep 10 2 1, dep 11 3 1]
      map (length . pePoints) (diagramEdges d) `shouldBe` [2, 2]

    it "centres a node over the two it depends on (reference image 1)" $ do
      -- 1 depends on 2 and 3, so 1 is drawn above both and centred.
      let d = layout cfg [node 1, node 2, node 3] [dep 10 2 1, dep 11 3 1]
      centreX (placed d 1)
        `shouldBe` (centreX (placed d 2) + centreX (placed d 3)) / 2

    it "keeps a single-dependency chain colinear (reference image 2)" $ do
      let d = layout cfg [node 1, node 2, node 3] [dep 10 2 1, dep 11 3 2]
      map (centreX . placed d) [1, 2, 3]
        `shouldSatisfy` \[a, b, c] -> a == b && b == c

    it "reports bounds that contain every node box" $ do
      let d = layout cfg (map node [1 .. 4]) [dep 10 2 1, dep 11 3 1, dep 12 4 3]
          Bounds mn mx = diagramBounds d
          within p =
            ptX (pnTopLeft p) >= ptX mn
              && ptY (pnTopLeft p) >= ptY mn
              && ptX (pnTopLeft p) + szW (pnSize p) <= ptX mx
              && ptY (pnTopLeft p) + szH (pnSize p) <= ptY mx
      all within (diagramNodes d) `shouldBe` True

    it "anchors on the project root when there is one" $ do
      let d = layout cfg [rootNode 1, node 2] [dep 10 2 1]
      diagramRootAnchor d `shouldSatisfy` isJust

    it "has no anchor when no node is a project root" $ do
      let d = layout cfg [node 1, node 2] [dep 10 2 1]
      diagramRootAnchor d `shouldBe` Nothing

    it "wraps labels to the configured box" $ do
      let long = T.pack (replicate 200 'a')
          d = layout cfg [(node 1) {lnLabel = long}] []
          ls = pnLines (placed d 1)
      length ls `shouldSatisfy` (<= cfgLabelLines cfg)
      all ((<= cfgLabelWidth cfg) . T.length) ls `shouldBe` True

    it "produces a diagram for a cyclic graph rather than failing" $ do
      let ns = map node [1 .. 3]
          es = [dep 10 2 1, dep 11 3 2, dep 12 1 3]
          d = layout cfg ns es
      length (diagramNodes d) `shouldBe` 3
      length (mapMaybe (Just . peId) (diagramEdges d)) `shouldBe` 3

    it "marks the edge that was reversed to break a cycle" $ do
      let d = layout cfg (map node [1 .. 2]) [dep 10 2 1, dep 11 1 2]
      length (filter peReversed (diagramEdges d)) `shouldBe` 1

    it "handles an empty graph" $ do
      let d = layout cfg [] []
      diagramNodes d `shouldBe` []
      diagramEdges d `shouldBe` []

    it "is deterministic" $ do
      let ns = map node [1 .. 5]
          es = [dep 10 2 1, dep 11 3 1, dep 12 4 2, dep 13 5 3]
      layout cfg ns es `shouldBe` layout cfg ns es
