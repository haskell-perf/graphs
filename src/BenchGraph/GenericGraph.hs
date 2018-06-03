module BenchGraph.GenericGraph
  (
  Vertex,
  Edge,
  Edges,
  Size,
  Name,
  GenericGraph
  )

where

import Data.List (nub)
import BenchGraph.Named

type Vertex = Int
type Edge   = (Vertex,Vertex)
type Edges  = [Edge]
type Size   = Int

-- | Generic graph with a name. An empty list of edges means a single vertex 0
-- The function MUST be called with an @Int >= 1@
type GenericGraph = Named (Size -> Edges)
