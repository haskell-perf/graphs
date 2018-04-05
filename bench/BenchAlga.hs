{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative ((<*>))

import Criterion.Main

import BenchGraph
import BenchGraph.Path

import Algebra.Graph

-- For example with alga
instance GraphImpl (Graph Int) where
  mkGraph = edges

-- A simple consummer
isEmpty' :: ToFuncToBench (Graph Int)
isEmpty' = const $ Consummer "IsEmpty" isEmpty

--A simple function
pathHasEdge :: ToFuncToBench (Graph Int)
pathHasEdge = FuncWithArg "hasEdge" (uncurry hasEdge) show . take 2 . edgesNotInPath

tenPowers :: [Int]
tenPowers = 1: map (10*) tenPowers

main :: IO ()
main = do
  let toTest = map benchFunc [isEmpty', pathHasEdge]
  defaultMain $ toTest <*> (map mkPath $ take 5 tenPowers)
