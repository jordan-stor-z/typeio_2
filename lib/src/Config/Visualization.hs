{-# LANGUAGE OverloadedStrings #-}

{- | Which visualization of the dependency graph the app renders.

See @docs/architecture/visualization-switching.md@. The value is read
once at startup and selects a handler when the container is built — not
per request, and not from a query parameter.
-}
module Config.Visualization
  ( keyVisualization
  , Visualization (..)
  , lookupVisualization
  ) where

import Data.Aeson (ToJSON, toJSON)
import System.Environment (lookupEnv)

keyVisualization :: String
keyVisualization = "GRAPH_VISUALIZATION"

{- | The visualizations that exist. Each has its own directory under
@Domain.Project.Visualization@ and owns its conversion and rendering;
the layered layout engine in @Domain.Project.Graph@ is shared
infrastructure either may use.

Parsed with 'Read', so the environment value is the constructor name —
@Layered@ or @Rootless@ — matching how @ENV@ already parses into
'Config.App.EnvironmentName'.
-}
data Visualization
  = -- | The layered orthogonal graph, project root included.
    Layered
  | {- | The same layered drawing with the project root left out, so the
    work is not forced to converge on it (#215).
    -}
    Rootless
  | {- | The orbital dependency-weighted drawing: radial, rootless, and
    with a shared dependency replicated into every work stream that
    waits on it, so the drawing contains no crossing edges at all
    (#229). Brings its own geometry — it is the first visualization
    that does not use the layered engine.
    -}
    Orbital
  deriving (Eq, Read, Show)

instance ToJSON Visualization where
  toJSON = toJSON . show

{- | The raw value, if set. Validation happens in 'Config.App' alongside
every other variable so one bad @.env@ reports every problem at once
rather than only the first.
-}
lookupVisualization :: IO (Maybe String)
lookupVisualization = lookupEnv keyVisualization
