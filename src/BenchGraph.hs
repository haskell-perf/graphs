{-# LANGUAGE ExistentialQuantification #-}

module BenchGraph (
  Suite (..),
  Algorithm (..), withNames,
  GraphImpl,
  benchmark,
  mkGraph,
  benchAlgorithm,
  consumer
) where

import Criterion.Main
import Control.DeepSeq (NFData(..), ($!!))

import BenchGraph.GenericGraph

type Name = String
type Size = Int

-- We pass the generic graph represented by 'Edges' to create an algorithm to test
data Suite g = Suite Name (Edges -> Algorithm g)

-- A graph algorithm operating on a graph type @g@, which takes an input of
-- type @i@ and producing an output of type @o@. Algorithms come with a list of
-- named inputs, all of which will be tried during benchmarking.
data Algorithm g = forall i o. NFData o => Algorithm (i -> g -> o) [(Name, i)]

-- Show items in a list
withNames :: Show a => [a] -> [(Name, a)]
withNames = map (\x -> (show x, x))

-- Allow a cleaner syntax
consumer :: NFData o => Name -> (g -> o) -> Suite g
consumer name f = Suite name $ const $ Algorithm (const f) [(name, ())]

-- An interface between our generic graphs and others
class GraphImpl g where
    mkGraph :: Edges -> g

-- Utilitary
benchmark :: (GraphImpl g, NFData g)
          => [(GenericGraph, [Int])] -> Suite g -> Benchmark
benchmark lists (Suite name mkAlgorithm) =
    bgroup name $ map (uncurry $ benchGraph mkAlgorithm) lists

-- Once we selected a graph, we can map over its arguments
benchGraph :: (GraphImpl g, NFData g)
           => (Edges -> Algorithm g) -> GenericGraph -> [Int] -> Benchmark
benchGraph algorithm g = bgroup (name g) . map (benchSuite algorithm g)

-- Main function
benchSuite :: (GraphImpl g, NFData g)
          => (Edges -> Algorithm g) -> GenericGraph -> Size -> Benchmark
benchSuite algorithm g n = benchAlgorithm (algorithm edges) (show n) $!! mkGraph edges
  where
    edges = mk g n

-- Here we bench a single function over a single graph
benchAlgorithm :: GraphImpl g => Algorithm g -> Name -> g -> Benchmark
benchAlgorithm (Algorithm f is) sizeInfo g =
    bgroup sizeInfo [ bench name $ nf (f i) g | (name, i) <- is ]
