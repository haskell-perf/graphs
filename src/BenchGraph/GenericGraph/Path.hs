module BenchGraph.GenericGraph.Path (
  path
) where

import BenchGraph.Named
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

path :: GenericGraph
path = ("Path",mkPath)

mkPath :: Int -> Edges
mkPath n = take n $ iterate ((\(x,y) -> (x+1,y+1)) :: (Int,Int) -> (Int,Int)) (0,1)

