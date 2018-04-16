module BenchGraph.Circuit (
  circuit
) where

import BenchGraph.Path
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

circuit :: GenericGraph
circuit = GenericGraph "Circuit" mkCircuit

mkCircuit :: Int -> Edges 
mkCircuit n = mk path n ++ [(n,0)]

