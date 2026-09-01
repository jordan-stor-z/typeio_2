{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Util where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)

intToText :: Integral a => a -> Text
intToText = toStrict . toLazyText . decimal

{- | Greedily wrap text to at most @maxLines@ lines of roughly @width@
characters each, truncating with an ellipsis if it doesn't fit.

Exists for the dependency graph's node labels, which render as SVG
text inside a fixed-radius circle: SVG has no text wrapping of its
own, so an unwrapped title renders as one long line far wider than
its node. Sizing the graph's spacing around those runaway lines is
what made the layout sprawl; wrapping them to the node instead keeps
the whole graph compact. The full title is always still available in
the node's detail panel, so truncating the displayed label loses
nothing.

A word longer than @width@ on its own is hard-split rather than
allowed to overflow the node.
-}
wrapLabel :: Int -> Int -> Text -> [Text]
wrapLabel width maxLines label
  | width <= 0 || maxLines <= 0 = []
  | otherwise = clamp . greedy . concatMap hardSplit . T.words $ label
  where
    -- Break a single over-long word into width-sized pieces, so it
    -- can't overflow the node on its own.
    hardSplit w
      | T.length w <= width = [w]
      | otherwise = T.chunksOf width w
    -- Standard greedy fill: keep adding words to the current line
    -- while they fit, then start a new one.
    greedy = foldl step []
    step [] w = [w]
    step acc w =
      let ln = last acc
       in if T.length ln + 1 + T.length w <= width
            then init acc ++ [ln <> " " <> w]
            else acc ++ [w]
    -- Past maxLines, keep the allowed lines and mark the last one as
    -- truncated rather than dropping the overflow silently.
    clamp ls
      | length ls <= maxLines = ls
      | otherwise =
          let kept = take maxLines ls
              lst = last kept
              room = max 0 (width - 1)
           in init kept ++ [T.take room lst <> "…"]
