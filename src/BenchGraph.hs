{-# LANGUAGE ExistentialQuantification #-}

module BenchGraph (
  ToFuncToBench,
  FuncToBench (..),
  GraphImpl,
  benchOver,
  mkGraph,
  benchFunc,
  extractMaxVertex
) where

import Criterion.Main
import Control.DeepSeq (NFData(..), ($!!))
import Control.Monad (ap)

import BenchGraph.GenericGraph

-- We want to pass the generic graph to create an according function to test
type ToFuncToBench a = Edges -> FuncToBench a

-- Type used to group different types of functions
data FuncToBench a = forall b. NFData b => Consummer String (a -> b) 
  | forall b c. NFData c => FuncWithArg String (b -> a -> c) (b -> String) [b]

-- An interface between our generic graphs and others
class GraphImpl g where
  mkGraph :: Edges -> g

-- Utilitary
benchOver :: (GraphImpl g, NFData g) => GenericGraph -> [ToFuncToBench g] -> [Int] -> [Benchmark]
benchOver gr tofuncs = ap (map (benchFunc gr ) tofuncs) 

-- Main function
benchFunc :: (GraphImpl g, NFData g) => GenericGraph -> ToFuncToBench g -> Int -> Benchmark
benchFunc gr tofunc n = benchFunc' (tofunc edges) (name gr ++ "/" ++ show n) $!! mkGraph edges
  where
    edges = mk gr n

-- Here we bench a single function over a single graph
benchFunc' :: GraphImpl g => FuncToBench g -> String -> g -> Benchmark
benchFunc' (Consummer name fun) ename graph = bench (name++"/"++ename) $ nf fun graph
benchFunc' (FuncWithArg name fun showArg args ) ename graph = bgroup (name++"/"++ename) $ map (\arg -> bench (showArg arg) $ nf (fun arg) graph) args

extractMaxVertex :: [(Int, Int)] -> Int
extractMaxVertex = foldl (\act (v1,v2) -> max act (max v1 v2)) 0

