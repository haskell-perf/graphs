module BenchGraph.GenericGraph.Clique (
  clique
) where

import BenchGraph.Named
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

clique :: GenericGraph
clique = ("Clique",mkClique)

mkClique :: Int -> Edges
mkClique n = concatMap (\cur -> (map (\x -> (cur, x)) [(cur+1)..n])) [0..n]

