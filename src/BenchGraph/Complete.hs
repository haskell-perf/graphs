module BenchGraph.Complete (
  mkComplete
) where

import Data.List (delete)
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

mkComplete :: Int -> GenericGraph
mkComplete n = GenericGraph ("complete"++(show n)) $ mkComplete' n n 

mkComplete' :: Int -> Int -> Edges
mkComplete' _ (-1) = []
mkComplete' n cur = (map (\x -> (cur, x)) [0..n]) ++ (mkComplete' n $ cur-1)

