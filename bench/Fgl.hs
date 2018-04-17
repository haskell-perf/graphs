{-# LANGUAGE FlexibleInstances #-}

module Fgl
(allBenchs)
where

import Criterion.Main

import BenchGraph
import BenchGraph.GenericGraph (vertices)

import BenchGraph.Utils

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

instance GraphImpl UGr where
  mkGraph e = mkUGraph (vertices e) e

isEmpty' :: ToFuncToBench UGr
isEmpty' = createConsumer "isEmpty" isEmpty

edgeList :: ToFuncToBench UGr
edgeList = createConsumer "edgeList" edges

vertexList :: ToFuncToBench UGr
vertexList = createConsumer "vertexList" nodes

hasEdge' :: ToFuncToBench UGr
hasEdge' = ToFuncToBench "hasEdge (not in graph)" $ FuncWithArg (flip hasEdge) show . take 2 . edgesNotInGraph 

allBenchs :: [Benchmark]
allBenchs = map (benchOver graphs) generics
  where
    generics = [hasEdge', isEmpty', edgeList, vertexList]

