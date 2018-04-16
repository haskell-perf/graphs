module BenchGraph.Complete (
  complete
) where

import BenchGraph.GenericGraph (Edges,GenericGraph(..))

complete :: GenericGraph
complete = GenericGraph "complete" mkComplete

mkComplete :: Int -> Edges 
mkComplete n = concatMap (\cur -> (map (\x -> (cur, x)) [0..(n-1)])) [0..(n-1)]

