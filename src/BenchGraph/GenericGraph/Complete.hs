module BenchGraph.GenericGraph.Complete (
  complete
) where

import BenchGraph.GenericGraph (Edges,GenericGraph)

-- | A complete graph is a graph where every vertex has an edge to all the vertices of the graph
complete :: GenericGraph
complete = ("Complete",mkComplete)

mkComplete :: Int -> Edges
mkComplete n = concatMap (\cur -> (map (\x -> (cur, x)) [0..(n-1)])) [0..(n-1)]

