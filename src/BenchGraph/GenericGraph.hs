module BenchGraph.GenericGraph
  (
  Vertex,
  Edge,
  Edges,
  Size,
  Name,
  GenericGraph,
  vertices
  )

where

import Data.List (nub)
import BenchGraph.Named

type Vertex = Int
type Edge   = (Vertex,Vertex)
type Edges  = [Edge]
type Size   = Int

-- Generic graph with a name
type GenericGraph = Named (Size -> Edges)

vertices :: Edges -> [Vertex]
vertices = nub . concat . unzip
