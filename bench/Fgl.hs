{-# LANGUAGE FlexibleInstances #-}

module Fgl
(allBenchs)
where

import Criterion.Main

import BenchGraph
import BenchGraph.Path
import BenchGraph.Complete
import BenchGraph.GenericGraph (vertices)

import BenchGraph.Utils

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

instance GraphImpl UGr where
  mkGraph e = mkUGraph (vertices e) e

isEmpty' :: ToFuncToBench UGr
isEmpty' = const $ Consummer "IsEmpty" isEmpty

edgeList :: ToFuncToBench UGr
edgeList = const $ Consummer "edgeList" edges

vertexList :: ToFuncToBench UGr
vertexList = const $ Consummer "vertexList" nodes

hasEdge' :: ToFuncToBench UGr
hasEdge' = FuncWithArg "hasEdge" (flip hasEdge) show 

allBenchs :: [Benchmark]
allBenchs = toTestComplete ++ toTestPath 
  where
    generics = [isEmpty', edgeList, vertexList]
    
    toTestPath = benchOver path ((hasEdge' . take 2 . edgesNotInPath) : generics) $ take 5 tenPowers

    toTestComplete = benchOver complete ((hasEdge' . take 3 ): generics) $ take 3 tenPowers
