module BenchGraph.GenericGraph(
Vertex,
Edges,
Size,
Name,
GenericGraph,
vertices,
mk
) where

import Data.List (nub)
import BenchGraph.Named

type Vertex = Int
type Edges  = [(Vertex,Vertex)]
type Size   = Int

-- Generic graph with a name
type GenericGraph = Named (Size -> Edges)

mk :: GenericGraph -> (Size -> Edges)
mk = obj

vertices :: Edges -> [Vertex]
vertices = nub . concat . unzip
