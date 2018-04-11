module BenchGraph.Path (
  mkPath,
  edgesNotInPath
) where

import BenchGraph.GenericGraph (Edges,GenericGraph(..))

mkPath :: Int -> GenericGraph
mkPath n = GenericGraph ("path"++(show n)) $ take n $ iterate ((\(x,y) -> (x+1,y+1)) :: (Int,Int) -> (Int,Int)) (0,1)

edgesNotInPath :: Edges -> Edges
edgesNotInPath = map (\(x,y)-> (x-1,y+1))
