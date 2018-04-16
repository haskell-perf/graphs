{-# LANGUAGE FlexibleInstances #-}

module Alga
(allBenchs)
where

import Criterion.Main

import BenchGraph
import BenchGraph.Path
import BenchGraph.Complete

import BenchGraph.Utils

import Algebra.Graph hiding (path)

-- For example with alga
instance GraphImpl (Graph Int) where
  mkGraph = edges

-- A simple consummer
isEmpty' :: ToFuncToBench (Graph Int)
isEmpty' = const $ Consumer "IsEmpty" isEmpty

vertexList' :: ToFuncToBench (Graph Int)
vertexList' = const $ Consumer "vertexList" vertexList 

edgeList' :: ToFuncToBench (Graph Int)
edgeList' = const $ Consumer "edgeList" edgeList

--A simple function
hasEdge' :: ToFuncToBench (Graph Int)
hasEdge' = FuncWithArg "hasEdge" (uncurry hasEdge) show

allBenchs :: [Benchmark]
allBenchs = toTestPath ++ toTestComplete
  where
    generics = [isEmpty', edgeList', vertexList']

    toTestPath = benchOver path ((hasEdge' . take 2 . edgesNotInPath) : generics) $ take 5 tenPowers

    toTestComplete = benchOver complete ((hasEdge' . take 3 ): generics) $ take 3 tenPowers

