module BenchGraph.Circuit (
  circuit
) where

import BenchGraph.Path
import BenchGraph.Named
import BenchGraph.GenericGraph (Edges,GenericGraph(..),mk)

circuit :: GenericGraph
circuit = Named "Circuit" mkCircuit

mkCircuit :: Int -> Edges
mkCircuit n = mk path n ++ [(n,0)]

