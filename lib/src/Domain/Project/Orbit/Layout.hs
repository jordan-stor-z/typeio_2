{- | Turning the unfolded forest into placed discs and links.

Phases 3-5 of
@docs/architecture/orbital-dependency-weighted-graph.md@ (#236), on top
of the forest "Domain.Project.Orbit.Unfold" produces.

The shape in one paragraph: leaves are the unit of angular space, so
each gets an equal slice of the circle and every stream ends up owning
one contiguous wedge; a parent sits at the mean of its children's
angles; rings move outward far enough that the discs on them clear each
other. Links are straight segments trimmed to the rims, with the
arrowhead on the inner end.

__Why this contains no crossing-reduction pass:__ there is nothing to
reduce. Every disc has exactly one dependent (that is what unfolding
buys) and every subtree owns a disjoint wedge, so no two links can
cross. The layered engine's 'Domain.Project.Graph.Order' exists because
a layered drawing of a DAG genuinely has crossings to minimise; here the
absence is structural. @LayoutSpec@ asserts it directly rather than
taking the argument's word for it.
-}
module Domain.Project.Orbit.Layout
  ( orbit
  , leafCount
  ) where

import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Util (wrapLabel)
import Domain.Project.Orbit.Types
  ( Bounds (..)
  , Disc (..)
  , Link (..)
  , NodeId
  , OrbitConfig (..)
  , OrbitDiagram (..)
  , OrbitEdge
  , OrbitNode (..)
  , OrbitTree (..)
  , Point (..)
  )
import Domain.Project.Orbit.Unfold (unfold)

{- | Lay a project's nodes and dependencies out as an orbital drawing.

__Total.__ It must produce a diagram for any input — an empty graph,
isolated nodes, duplicate edges, an edge naming a node that is not
present, or a cycle. It never fails and never refuses to draw, on the
same terms as the layered engine's
'Domain.Project.Graph.Layout.layout'.
-}
orbit :: OrbitConfig -> [OrbitNode] -> [OrbitEdge] -> OrbitDiagram
orbit cfg ns es =
  OrbitDiagram
    { odDiscs = placed
    , odLinks = links
    , odBounds = bounds cfg placed
    }
  where
    forest = unfold ns es
    labels = M.fromList [(onId n, onLabel n) | n <- ns]

    angled = angles forest
    radii = ringRadii cfg (length forest) angled
    placed = map (place cfg labels radii) (flattenAngled angled)
    links = concatMap (treeLinks cfg radii) angled

{- | Leaves of a subtree, which is the angular space it is owed.

A leaf is a disc waiting on nothing, and it is the unit of angular
space because it is what actually has to fit side by side on the
outside of the drawing. Weighting by disc count instead would give a
deep chain the same room as a wide fan, and the fan is what needs it.
-}
leafCount :: OrbitTree -> Int
leafCount t
  | null (otChildren t) = 1
  | otherwise = sum (map leafCount (otChildren t))

{- | An angle for every disc, in radians clockwise from 12 o'clock.

Each leaf takes an equal slice of the circle and sits at its centre; a
parent takes the __mean of its children's angles__. An even number of
children therefore centres a parent between them, which is the
commonest shape and the one an alternative rule would visibly get wrong
— the same argument 'Domain.Project.Graph.Coord' makes for averaging
the middle two neighbours rather than picking one.

Because a mean of angles is a convex combination of them, a disc always
lands strictly inside its own subtree's wedge. That is what makes the
drawing planar, and @LayoutSpec@ asserts it rather than assuming it.
-}
angles :: [OrbitTree] -> [Angled]
angles forest = snd (goMany (0 :: Int) forest)
  where
    total = max 1 (sum (map leafCount forest))
    slice = 2 * pi / fromIntegral total

    goMany i [] = (i, [])
    goMany i (t : rest) =
      let (i', a) = go i t
          (i'', as) = goMany i' rest
       in (i'', a : as)

    go i t = case otChildren t of
      [] ->
        ( i + 1
        , Angled t ((fromIntegral i + 0.5) * slice) []
        )
      kids ->
        let (i', as) = goMany i kids
            theta = sum (map anAngle as) / fromIntegral (length as)
         in (i', Angled t theta as)

-- | A tree with an angle on every node, before radii are known.
data Angled = Angled
  { anTree :: OrbitTree
  , anAngle :: Double
  , anChildren :: [Angled]
  }

-- | Every angled disc in a forest, parents before children.
flattenAngled :: [Angled] -> [Angled]
flattenAngled = concatMap (\a -> a : flattenAngled (anChildren a))

{- | Radius per ring, outward from the eye.

@
r0 = eyeRadius                                   (0 with a single head)
rk = max (r(k-1) + minRingGap)
         ((2*discRadius + discGap) / minAngularGap k)
@

__Rings are deliberately not evenly spaced.__ The angular gap between
neighbours shrinks as subtrees subdivide, so an outer ring generally
needs more room than the minimum; deriving each radius from the demand
on that particular ring keeps the eye small on a shallow project
instead of sizing the whole drawing for its worst ring. The layered
engine spaces its rows the same way, and for the same reason.

The trivially-correct alternative — one radius wide enough that /every/
leaf could sit on the innermost ring, applied to all of them — is
rejected for producing an enormous empty middle on any project with
many leaves.

__The single-head case.__ A forest of one tree has no meaningful
angular mean for its root: its subtree spans the whole circle, and the
mean of angles spread over 2π is degenerate. So a lone head is placed
at the centre and the rings shift outward by one. This is the only case
where the eye is occupied, and it is not a rare shape — a project whose
work all converges on one deliverable is ordinary. The rule "the centre
is empty" describes what happens with more than one stream, which is
what makes an empty eye meaningful: it says these streams are
independent.
-}
ringRadii :: OrbitConfig -> Int -> [Angled] -> M.Map Int Double
ringRadii cfg treeCount as = foldl step M.empty [0 .. maxRing]
  where
    everyDisc = flattenAngled as
    maxRing =
      if null everyDisc
        then -1
        else maximum (map (otRing . anTree) everyDisc)

    singleHead = treeCount == 1

    step acc k = M.insert k r acc
      where
        -- Centre to centre: the two rims plus the clear space between
        -- them. Adding only the gap would leave radially adjacent discs
        -- tangent, with a zero-length link between them (#238).
        ringStep = 2 * cfgDiscRadius cfg + cfgMinRingGap cfg
        prev = maybe start (+ ringStep) (M.lookup (k - 1) acc)
        start
          | singleHead = 0
          | otherwise = cfgEyeRadius cfg
        r = max prev (demand k)

    -- The radius at which this ring's tightest neighbouring pair
    -- clears. A ring with one disc on it imposes nothing.
    demand k = case minGapOn k of
      Nothing -> 0
      Just g
        | g <= 0 -> 0
        | otherwise -> (2 * cfgDiscRadius cfg + cfgDiscGap cfg) / g

    minGapOn k = case sort [anAngle a | a <- everyDisc, otRing (anTree a) == k] of
      [] -> Nothing
      [_] -> Nothing
      ts ->
        -- The ring is a circle, so the gap from the last disc back
        -- round to the first counts too.
        Just (minimum (zipWith (-) (tail ts) ts <> [2 * pi - (last ts - head ts)]))

place :: OrbitConfig -> M.Map NodeId T.Text -> M.Map Int Double -> Angled -> Disc
place cfg labels radii a =
  Disc
    { dNode = otNode t
    , dReplica = otReplica t
    , dRing = otRing t
    , dAngle = anAngle a
    , dCentre = polar (radiusOf radii (otRing t)) (anAngle a)
    , dLines =
        wrapLabel
          (cfgLabelWidth cfg)
          (cfgLabelLines cfg)
          (M.findWithDefault T.empty (otNode t) labels)
    }
  where
    t = anTree a

radiusOf :: M.Map Int Double -> Int -> Double
radiusOf radii k = fromMaybe 0 (M.lookup k radii)

{- | Clockwise from 12 o'clock, so the first stream sits at the top of
the drawing where a reader looks first. y grows downward, per SVG.
-}
polar :: Double -> Double -> Point
polar r theta = Point (r * sin theta) (negate (r * cos theta))

{- | One link per parent-child pair, trimmed to both rims.

The arrowhead goes on 'lTo', the inner end — the dependent, the one
waiting.
-}
treeLinks :: OrbitConfig -> M.Map Int Double -> Angled -> [Link]
treeLinks cfg radii a =
  [ trim cfg (centre k) (centre a)
  | k <- anChildren a
  ]
    <> concatMap (treeLinks cfg radii) (anChildren a)
  where
    centre x = polar (radiusOf radii (otRing (anTree x))) (anAngle x)

{- | The segment between two centres, pulled back to each rim.

Falls back to the bare centres when the two discs are closer together
than their own radii — degenerate, and only reachable if a
configuration makes discs bigger than the rings separating them, but
'orbit' is total and a @NaN@ coordinate would poison the whole drawing.
-}
trim :: OrbitConfig -> Point -> Point -> Link
trim cfg from to
  | dist <= 2 * r = Link from to
  | otherwise =
      Link
        (Point (ptX from + ux * r) (ptY from + uy * r))
        (Point (ptX to - ux * r) (ptY to - uy * r))
  where
    r = cfgDiscRadius cfg
    dx = ptX to - ptX from
    dy = ptY to - ptY from
    dist = sqrt (dx * dx + dy * dy)
    ux = if dist == 0 then 0 else dx / dist
    uy = if dist == 0 then 0 else dy / dist

{- | Everything the drawing occupies, plus a margin. An empty drawing is
a point at the origin — @handleGraphWith@ answers 403 before a project
with no nodes ever reaches layout, but 'orbit' still has to return
something.
-}
bounds :: OrbitConfig -> [Disc] -> Bounds
bounds cfg ds
  | null ds = Bounds (Point 0 0) (Point 0 0)
  | otherwise =
      Bounds
        (Point (minimum xs - pad) (minimum ys - pad))
        (Point (maximum xs + pad) (maximum ys + pad))
  where
    pad = cfgDiscRadius cfg + cfgMargin cfg
    xs = map (ptX . dCentre) ds
    ys = map (ptY . dCentre) ds
