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
isEmpty' :: Suite (Graph Int)
isEmpty' = consumer "isEmpty" isEmpty

vertexList' :: Suite (Graph Int)
vertexList' = consumer "vertexList" vertexList

edgeList' :: Suite (Graph Int)
edgeList' = consumer "edgeList" edgeList

--A simple function
hasEdge' :: Suite (Graph Int)
hasEdge' = Suite "hasEdge (not in graph)" $
    Algorithm (uncurry hasEdge) . withNames . take 2 . edgesNotInGraph

allBenchs :: [Benchmark]
allBenchs = toTest
  where
    generics = [hasEdge', isEmpty', edgeList', vertexList']

    toTest = map (benchmark graphs) generics
