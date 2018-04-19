{-# LANGUAGE FlexibleInstances #-}

module Alga
(allBenchs)
where

import Criterion.Main

import BenchGraph

import BenchGraph.Utils

import Algebra.Graph

-- For example with alga
instance GraphImpl (Graph Int) where
  mkGraph = edges

-- A simple consummer
isEmpty' :: ToFuncToBench (Graph Int)
isEmpty' = createConsumer "isEmpty" isEmpty

vertexList' :: ToFuncToBench (Graph Int)
vertexList' = createConsumer "vertexList" vertexList

edgeList' :: ToFuncToBench (Graph Int)
edgeList' = createConsumer "edgeList" edgeList

--A simple function
hasEdge' :: ToFuncToBench (Graph Int)
hasEdge' = ToFuncToBench "hasEdge (not in graph)" $
    FuncToBench (uncurry hasEdge) . withNames . take 2 . edgesNotInGraph

allBenchs :: [Benchmark]
allBenchs = toTest
  where
    generics = [hasEdge', isEmpty', edgeList', vertexList']

    toTest = map (benchOver graphs) generics
