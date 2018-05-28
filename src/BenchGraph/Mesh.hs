module BenchGraph.Mesh (
  mesh
) where

import BenchGraph.Named
import BenchGraph.Path
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

import Control.Comonad (extract)

mesh :: GenericGraph
mesh = Named "Mesh" mkMesh

-- | Construct a Mesh with @n+1@ vertices
mkMesh :: Int -> Edges
mkMesh n = if n == 1
              then [(0,1)]
           else concatMap
              (\x -> let first = if (x+1) `mod` sq == 0 then [] else [(x,x+1)]
                         second = if x+sq >= sq^2 then [] else [(x,x+sq)]
                     in first ++ second
              )
              [0..(sq^2)]
  where
    sq = round $ sqrt $ fromRational $ toRational n
