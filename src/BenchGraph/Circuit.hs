module BenchGraph.Circuit (
  mkCircuit,
  edgesNotInCircuit
) where

import BenchGraph.Path
import BenchGraph (Edges,GenericGraph(..))

mkCircuit :: Int -> GenericGraph
mkCircuit n = GenericGraph ("cicruit"++(show n)) $ path ++ [(n,0)]
  where
    GenericGraph _ path = mkPath n

edgesNotInCircuit :: Edges -> Edges
edgesNotInCircuit = edgesNotInPath
