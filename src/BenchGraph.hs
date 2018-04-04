{-# LANGUAGE ExistentialQuantification #-}

module BenchGraph (
  Edges,
  GenericGraph (..),
  ToFuncToBench,
  FuncToBench (..),
  GraphImpl,
  mkGraph,
  benchFunc
) where

import Criterion.Main
import Control.DeepSeq (NFData(..))

type Edges = [(Int,Int)]

-- Generic graph with a name
data GenericGraph = GenericGraph String Edges

-- We want to pass the generic graph to create an according function to test
type ToFuncToBench a = Edges -> FuncToBench a

-- Type used to group different types of functions
data FuncToBench a = forall b. NFData b => Consummer String (a -> b) 
  | forall b c. NFData c => FuncWithArg String (b -> a -> c) (b -> String) [b]

-- An interface between our generic graphs and others
class GraphImpl g where
  mkGraph :: Edges -> g

-- Main function
-- Will be cooler if its return a single benchmark with bgroup
benchFunc :: GraphImpl g => ToFuncToBench g -> GenericGraph -> Benchmark
benchFunc tofunc (GenericGraph ename edges) = benchFunc' (tofunc edges) ename edges

-- Here we bench a single function over a single graph
benchFunc' :: GraphImpl g => FuncToBench g -> String -> Edges -> Benchmark
benchFunc' (Consummer name fun) ename edges = bench (name++"/"++ename) $ nf fun $! mkGraph edges
benchFunc' (FuncWithArg name fun showArg args ) ename edges = bgroup (name++"/"++ename) $ map (\arg -> bench (showArg arg) $ nf (fun arg) $! mkGraph edges) args

