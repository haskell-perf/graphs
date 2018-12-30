module BenchGraph.Types (
  GraphImpl (..)
) where

import BenchGraph.GenericGraph

-- An interface between our generic graphs and others
class GraphImpl g where
  mkGraph :: Edges -> g
  mkVertex :: g -- | A single vertex
