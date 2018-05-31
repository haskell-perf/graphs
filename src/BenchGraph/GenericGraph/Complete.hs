module BenchGraph.GenericGraph.Complete (
  complete
) where

import BenchGraph.Named
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

complete :: GenericGraph
complete = ("Complete",mkComplete)

mkComplete :: Int -> Edges
mkComplete n = concatMap (\cur -> (map (\x -> (cur, x)) [0..n])) [0..n]

