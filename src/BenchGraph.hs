{-# LANGUAGE ExistentialQuantification #-}

module BenchGraph (
  ToFuncToBench (..),
  FuncToBench (..), withNames,
  GraphImpl,
  benchOver,
  mkGraph,
  benchFunc,
  createConsumer
) where

import Criterion.Main
import Control.DeepSeq (NFData(..), ($!!))

import BenchGraph.GenericGraph

type Name = String

-- We want to pass the generic graph to create an according function to test
data ToFuncToBench a = ToFuncToBench Name (Edges -> FuncToBench a)

data FuncToBench a = forall i o. NFData o => FuncToBench (i -> a -> o) [(Name, i)]

withNames :: Show a => [a] -> [(Name, a)]
withNames = map (\x -> (show x, x))

-- Allow a cleaner syntax
createConsumer :: (NFData b)  => String -> (a->b) -> ToFuncToBench a
createConsumer str f = ToFuncToBench str $ const $ FuncToBench (\() -> f) [(str, ())]

-- An interface between our generic graphs and others
class GraphImpl g where
  mkGraph :: Edges -> g

-- Utilitary
benchOver :: (GraphImpl g, NFData g) => [(GenericGraph,[Int])] -> ToFuncToBench g -> Benchmark
benchOver lists (ToFuncToBench name mkFuncToBench) = bgroup name $ map (uncurry $ benchOver' mkFuncToBench) lists

-- Once we selected a graph, we can map over its arguments
benchOver' :: (GraphImpl g, NFData g) => (Edges -> FuncToBench g) -> GenericGraph -> [Int] -> Benchmark
benchOver' func gr ints = bgroup (name gr) $ map (benchFunc func gr) ints

-- Main function
benchFunc :: (GraphImpl g, NFData g) => (Edges -> FuncToBench g) -> GenericGraph -> Int -> Benchmark
benchFunc func gr n = benchFunc' (func edges) (show n) $!! mkGraph edges
  where
    edges = mk gr n

-- Here we bench a single function over a single graph
benchFunc' :: GraphImpl g => FuncToBench g -> Name -> g -> Benchmark
benchFunc' (FuncToBench f is) name graph =
    bgroup name $ map (\(s, i) -> bench s $ nf (f i) graph) is

