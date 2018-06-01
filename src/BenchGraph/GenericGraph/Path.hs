module BenchGraph.GenericGraph.Path (
  path
) where

import BenchGraph.Named
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

-- | A path is a graph like [(0,1),(1,2),(2,3)..(n,n+1)]
path :: GenericGraph
path = ("Path",mkPath)

mkPath :: Int -> Edges
mkPath n = take n $ iterate ((\(x,y) -> (x+1,y+1)) :: (Int,Int) -> (Int,Int)) (0,1)

