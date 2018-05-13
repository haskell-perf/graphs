module BenchGraph.Circuit (
  circuit
) where

import BenchGraph.Path
import BenchGraph.Named
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

circuit :: GenericGraph
circuit = Named "Circuit" mkCircuit

mkCircuit :: Int -> Edges
mkCircuit n = obj path n ++ [(n,0)]

