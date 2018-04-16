module BenchGraph.Utils (
tenPowers,
edgesNotInGraph,
extractMaxVertex
)
where

import Data.List ((\\))
import BenchGraph.GenericGraph
import BenchGraph.Complete

tenPowers :: [Int]
tenPowers = iterate (10*) 1 

-- | Remove given edges from the complete graph
edgesNotInGraph :: Edges -> Edges
edgesNotInGraph edgs = (\\) (mk complete  $ extractMaxVertex edgs) edgs

extractMaxVertex :: Edges -> Int
extractMaxVertex = foldl (\act (v1,v2) -> max act (max v1 v2)) 0
