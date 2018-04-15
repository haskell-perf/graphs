{-# LANGUAGE FlexibleInstances #-}

module Alga
(allBenchs)
where

import Criterion.Main

import BenchGraph
import BenchGraph.Path
import BenchGraph.Complete

import BenchGraph.Utils

import Algebra.Graph

-- For example with alga
instance GraphImpl (Graph Int) where
  mkGraph = edges

-- A simple consummer
isEmpty' :: ToFuncToBench (Graph Int)
isEmpty' = const $ Consummer "IsEmpty" isEmpty

vertexList' :: ToFuncToBench (Graph Int)
vertexList' = const $ Consummer "vertexList" vertexList 

edgeList' :: ToFuncToBench (Graph Int)
edgeList' = const $ Consummer "edgeList" edgeList

--A simple function
hasEdge' :: ToFuncToBench (Graph Int)
hasEdge' = FuncWithArg "hasEdge" (uncurry hasEdge) show

allBenchs :: [Benchmark]
allBenchs = completeB ++ pathB
  where
    generics = [isEmpty', edgeList', vertexList']

    toTestPath = map benchFunc $ (hasEdge' . take 2 . edgesNotInPath) : generics
    pathB = toTestPath <*> map mkPath (take 5 tenPowers)

    toTestComplete = map benchFunc $ (hasEdge' . take 3 ): generics
    completeB = toTestComplete <*> map mkComplete (take 3 tenPowers)

