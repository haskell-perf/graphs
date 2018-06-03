module BenchGraph.GenericGraph.Clique (
  clique
) where

import BenchGraph.Named
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

-- | A clique is a graph such that every two distinct vertices are adjacent
clique :: GenericGraph
clique = ("Clique",mkClique)

mkClique :: Int -> Edges
mkClique n = concatMap (\cur -> (map (\x -> (cur, x)) [(cur+1)..(n-1)])) [0..(n-1)]

