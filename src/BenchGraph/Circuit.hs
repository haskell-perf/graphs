module BenchGraph.Circuit (
  circuit,
  edgesNotInCircuit
) where

import BenchGraph.Path
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

circuit :: GenericGraph
circuit = GenericGraph "Circuit" mkCircuit

mkCircuit :: Int -> Edges 
mkCircuit n = mk path n ++ [(n,0)]

edgesNotInCircuit :: Edges -> Edges
edgesNotInCircuit = edgesNotInPath
