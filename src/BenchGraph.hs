{-# LANGUAGE ExistentialQuantification #-}

module BenchGraph (
  ToFuncToBench (..),
  FuncToBench (..),
  GraphImpl,
  benchOver,
  mkGraph,
  benchFunc,
  createConsumer
) where

import Criterion.Main
import Control.DeepSeq (NFData(..), ($!!))
import Control.Monad (ap)

import BenchGraph.GenericGraph

-- We want to pass the generic graph to create an according function to test
data ToFuncToBench a = ToFuncToBench String (Edges -> FuncToBench a)

-- Type used to group different types of functions
data FuncToBench a = forall b. NFData b => Consumer (a -> b) 
  | forall b c. NFData c => FuncWithArg (b -> a -> c) (b -> String) [b]

-- Allow a cleaner syntax
createConsumer :: (NFData b)  => String -> (a->b) -> ToFuncToBench a
createConsumer str f = ToFuncToBench str $ const $ Consumer f 

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
benchFunc' :: GraphImpl g => FuncToBench g -> String -> g -> Benchmark
benchFunc' (Consumer fun) ename graph = bench ename $ nf fun graph
benchFunc' (FuncWithArg fun showArg args ) ename graph = bgroup ename $ map (\arg -> bench (showArg arg) $ nf (fun arg) graph) args

