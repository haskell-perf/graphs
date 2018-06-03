module BenchGraph.GenericGraph.Circuit (
  circuit
) where

import BenchGraph.GenericGraph.Path
import BenchGraph.Named
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

-- | A circuit is a path closed on himself
circuit :: GenericGraph
circuit = ("Circuit",mkCircuit)

mkCircuit :: Int -> Edges
mkCircuit n = snd path n ++ [(n-1,0)]

