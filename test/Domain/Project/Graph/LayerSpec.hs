module Domain.Project.Graph.LayerSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Domain.Project.Graph.Layer (Arc (..), assignLayers, breakCycles)
import Domain.Project.Graph.Types
  ( EdgeId (..)
  , LayoutEdge (..)
  , LayoutNode (..)
  , NodeId (..)
  , NodeKind (..)
  )
import Test.Hspec

-- | @node n@ is a work node whose id and label are both derived from n.
node :: Int -> LayoutNode
node n = LayoutNode (NodeId (fromIntegral n)) WorkNode (T.pack ("node " <> show n))

{- | @dep i a b@: node @b@ depends on node @a@, so @a@ must finish first
and @a@ is drawn below @b@.
-}
dep :: Int -> Int -> Int -> LayoutEdge
dep i a b =
  LayoutEdge
    { leId = EdgeId (fromIntegral i)
    , leDependency = NodeId (fromIntegral a)
    , leDependent = NodeId (fromIntegral b)
    }

layerOf :: M.Map NodeId Int -> Int -> Int
layerOf m n = M.findWithDefault (-1) (NodeId (fromIntegral n)) m

layersFor :: [LayoutNode] -> [LayoutEdge] -> M.Map NodeId Int
layersFor ns es = assignLayers ns (breakCycles ns es)

spec :: Spec
spec = do
  describe "breakCycles" $ do
    it "orients an edge with the dependent above its dependency" $ do
      let ns = [node 1, node 2]
          -- 1 depends on 2: 2 must finish first, so 1 is above.
          arcs = breakCycles ns [dep 10 2 1]
      map (\a -> (arcFrom a, arcTo a, arcReversed a)) arcs
        `shouldBe` [(NodeId 1, NodeId 2, False)]

    it "reverses exactly one edge of a two-node cycle" $ do
      let ns = [node 1, node 2]
          arcs = breakCycles ns [dep 10 2 1, dep 11 1 2]
      length (filter arcReversed arcs) `shouldBe` 1

    it "treats a self-dependency as a cycle" $ do
      let arcs = breakCycles [node 1] [dep 10 1 1]
      map arcReversed arcs `shouldBe` [True]

    it "is deterministic for the same input" $ do
      let ns = [node 1, node 2, node 3]
          es = [dep 10 2 1, dep 11 3 2, dep 12 1 3]
      breakCycles ns es `shouldBe` breakCycles ns es

  describe "assignLayers" $ do
    it "puts a node nothing depends on in row 0" $ do
      let ls = layersFor [node 1, node 2] [dep 10 2 1]
      layerOf ls 1 `shouldBe` 0

    it "puts a dependency one row below its dependent" $ do
      let ls = layersFor [node 1, node 2] [dep 10 2 1]
      layerOf ls 2 `shouldBe` 1

    it "layers a chain by depth from the top" $ do
      -- 1 depends on 2, 2 depends on 3.
      let ls = layersFor [node 1, node 2, node 3] [dep 10 2 1, dep 11 3 2]
      map (layerOf ls) [1, 2, 3] `shouldBe` [0, 1, 2]

    it "puts siblings in the same row" $ do
      -- 1 depends on both 2 and 3.
      let ls = layersFor [node 1, node 2, node 3] [dep 10 2 1, dep 11 3 1]
      map (layerOf ls) [2, 3] `shouldBe` [1, 1]

    it "takes the longest path when a node is reachable two ways" $ do
      -- 1 depends on 2 and on 3; 2 depends on 3. 3 must sit below 2.
      let ls = layersFor [node 1, node 2, node 3] [dep 10 2 1, dep 11 3 1, dep 12 3 2]
      map (layerOf ls) [1, 2, 3] `shouldBe` [0, 1, 2]

    it "gives every node a layer, including isolated ones" $ do
      let ns = [node 1, node 2, node 3]
          ls = layersFor ns [dep 10 2 1]
      M.size ls `shouldBe` 3
      layerOf ls 3 `shouldBe` 0

    it "layers disconnected components independently" $ do
      let ns = [node 1, node 2, node 3, node 4]
          ls = layersFor ns [dep 10 2 1, dep 11 4 3]
      map (layerOf ls) [1, 2, 3, 4] `shouldBe` [0, 1, 0, 1]

    it "terminates and layers everything despite a cycle" $ do
      -- The database permits cycles and nothing validates them away
      -- yet, so this must not hang or drop nodes.
      let ns = [node 1, node 2, node 3]
          ls = layersFor ns [dep 10 2 1, dep 11 3 2, dep 12 1 3]
      M.size ls `shouldBe` 3

    it "handles an empty graph" $
      layersFor [] [] `shouldBe` M.empty
