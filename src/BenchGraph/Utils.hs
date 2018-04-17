module BenchGraph.Utils (
tenPowers,
edgesNotInGraph,
extractMaxVertex,
graphs
)
where

import Data.List ((\\))
import BenchGraph.GenericGraph
import BenchGraph.Complete
import BenchGraph.Circuit
import BenchGraph.Path

tenPowers :: [Int]
tenPowers = iterate (10*) 1 

-- | Remove given edges from the complete graph
edgesNotInGraph :: Edges -> Edges
edgesNotInGraph edgs = (\\) (mk complete  $ extractMaxVertex edgs) edgs

extractMaxVertex :: Edges -> Int
extractMaxVertex = foldl (\act (v1,v2) -> max act (max v1 v2)) 0

graphs :: [(GenericGraph, [Int])]
graphs = [
  (path, take 5 tenPowers),
  (circuit, take 5 tenPowers),
  (complete, take 3 tenPowers)
  ]
