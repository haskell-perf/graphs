module BenchGraph.Complete (
  mkComplete
) where

import BenchGraph.GenericGraph (Edges,GenericGraph(..))

mkComplete :: Int -> GenericGraph
mkComplete n = GenericGraph ("complete"++(show n)) $ mkComplete' n 

mkComplete' :: Int -> Edges
mkComplete' n = concatMap (\cur -> (map (\x -> (cur, x)) [0..(n-1)])) [0..(n-1)]

