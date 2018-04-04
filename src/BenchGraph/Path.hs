module BenchGraph.Path (
  mkPath,
  edgesNotInPath
) where

import BenchGraph

mkPath :: Int -> Edges
mkPath n = take n $ iterate ((\(x,y) -> (x+1,y+1)) :: (Int,Int) -> (Int,Int)) (0,1)

edgesNotInPath :: Edges -> Edges
edgesNotInPath = map (\(x,y)-> (x-1,y+1))
