module BenchGraph.GenericGraph.Mesh (
  mesh
) where

import BenchGraph.Named
import BenchGraph.GenericGraph.Path
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

mesh :: GenericGraph
mesh = ("Mesh",mkMesh)

-- | Construct a Mesh with @n@ vertices
mkMesh :: Int -> Edges
mkMesh n = if n == 1
              then []
           else filter (\(x,y) -> x < n && y < n) $ concatMap
              (\x -> let first = if (x+1) `mod` sq == 0 then [] else [(x,x+1)]
                         second = if x+sq >= sq^2 then [] else [(x,x+sq)]
                     in first ++ second
              )
              [0..(sq^2)]
  where
    sq = 1 + sq'
    sq' = round $ sqrt $ fromRational $ toRational n
