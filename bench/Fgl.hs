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

verticexList :: ToFuncToBench UGr
verticexList = const $ Consummer "vertexList" nodes

hasEdge' :: ToFuncToBench UGr
hasEdge' = FuncWithArg "hasEdge" (flip hasEdge) show 

allBenchs :: [Benchmark]
allBenchs = pathB ++ completeB 
  where
    generics = [isEmpty', edgeList, verticexList]

    toTestPath = map benchFunc $ (hasEdge' . take 2 . edgesNotInPath) : generics
    pathB = toTestPath <*> map mkPath (take 5 tenPowers)

    toTestComplete = map benchFunc $ (hasEdge' . take 3 ): generics
    completeB = toTestComplete <*> map mkComplete (take 3 tenPowers)
