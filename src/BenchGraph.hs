{-# LANGUAGE ExistentialQuantification #-}

module BenchGraph (
  Suite (..),
  withNames,
  GraphImpl,
  benchmark,
  mkGraph,
  benchAlgorithm,
  consumer
) where

import Criterion.Main
import Control.DeepSeq (NFData(..), ($!!))

import BenchGraph.GenericGraph

-- A graph algorithm operates on a graph type @g@, which takes an input of
-- type @i@ and produces an output of type @o@. Algorithms come with a list of
-- named inputs, all of which will be tried during benchmarking.
data Suite g = forall i o. NFData o => Suite
    { suiteName :: Name
    , algorithm :: i -> g -> o
    , inputs    :: Edges -> [(Name, i)] }

-- Show items in a list
withNames :: Show a => [a] -> [(Name, a)]
withNames = map (\x -> (show x, x))

-- Allow a cleaner syntax
consumer :: NFData o => Name -> (g -> o) -> Suite g
consumer name f = Suite name (const f) (const [("", ())])

-- An interface between our generic graphs and others
class GraphImpl g where
    mkGraph :: Edges -> g

-- Utilitary
benchmark :: (GraphImpl g, NFData g)
          => [(GenericGraph, [Size])] -> Suite g -> Benchmark
benchmark lists (Suite name algorithm inputs) =
    bgroup name $ map (uncurry $ benchGraph algorithm inputs) lists

-- Once we selected a graph, we can map over its arguments
benchGraph :: (GraphImpl g, NFData g, NFData o)
    => (i -> g -> o) -> (Edges -> [(Name, i)]) -> GenericGraph -> [Size] -> Benchmark
benchGraph algorithm inputs g = bgroup (name g) . map (benchSuite algorithm inputs g)

-- Main function
benchSuite :: (GraphImpl g, NFData g, NFData o)
    => (i -> g -> o) -> (Edges -> [(Name, i)]) -> GenericGraph -> Size -> Benchmark
benchSuite algorithm inputs g n = benchAlgorithm algorithm (inputs edges) (show n) $!! mkGraph edges
  where
    edges = mk g n

-- Here we bench a single function over a single graph
benchAlgorithm :: (GraphImpl g, NFData o) => (i -> g -> o) -> [(Name, i)] -> Name -> g -> Benchmark
benchAlgorithm algorithm inputs sizeInfo g =
    bgroup sizeInfo [ bench name $ nf (algorithm i) g | (name, i) <- inputs ]
