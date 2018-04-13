module BenchGraph.GenericGraph(
Vertex,
Edges,
GenericGraph (..),
vertices
) where

import Data.List (nub)

type Vertex = Int
type Edges = [(Vertex,Vertex)]

-- Generic graph with a name
data GenericGraph = GenericGraph String Edges deriving (Show)

vertices :: Edges -> [Vertex]
vertices = nub . concat . unzip 
