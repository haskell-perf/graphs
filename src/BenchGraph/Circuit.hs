module BenchGraph.Circuit (
  circuit
) where

import BenchGraph.Path
import BenchGraph.Named
import Control.Comonad (extract)
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

circuit :: GenericGraph
circuit = ("Circuit",mkCircuit)

mkCircuit :: Int -> Edges
mkCircuit n = extract path n ++ [(n,0)]

