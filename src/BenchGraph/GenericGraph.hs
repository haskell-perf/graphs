module BenchGraph.GenericGraph(
Vertex,
Edges,
Size,
Name,
GenericGraph (..),
vertices
) where

import Data.List (nub)

type Vertex = Int
type Edges  = [(Vertex,Vertex)]
type Size   = Int
type Name   = String

-- Generic graph with a name
data GenericGraph = GenericGraph {
  name :: Name,
  mk   :: Size -> Edges
}

vertices :: Edges -> [Vertex]
vertices = nub . concat . unzip
