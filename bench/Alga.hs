{-# LANGUAGE FlexibleInstances #-}

module Alga
(allBenchs)
where

import Control.Applicative ((<*>))

import Criterion.Main

import BenchGraph
import BenchGraph.Path
import BenchGraph.Complete

import Algebra.Graph

-- For example with alga
instance GraphImpl (Graph Int) where
  mkGraph = edges

-- A simple consummer
isEmpty' :: ToFuncToBench (Graph Int)
isEmpty' = const $ Consummer "IsEmpty" isEmpty

vertexCount' :: ToFuncToBench (Graph Int)
vertexCount' = const $ Consummer "vertexCount" vertexCount

edgeCount' :: ToFuncToBench (Graph Int)
edgeCount' = const $ Consummer "edgeCount" edgeCount

--A simple function
hasEdge' :: ToFuncToBench (Graph Int)
hasEdge' = FuncWithArg "hasEdge" (uncurry hasEdge) show

tenPowers :: [Int]
tenPowers = 1: map (10*) tenPowers

allBenchs :: [Benchmark]
allBenchs = completeB ++ pathB
  where
    generics = [isEmpty', vertexCount', edgeCount']

    toTestPath = map benchFunc $ (hasEdge' . take 2 . edgesNotInPath) : generics
    pathB = toTestPath <*> map mkPath (take 5 tenPowers)

    toTestComplete = map benchFunc $ (hasEdge' . take 3 ): generics
    completeB = toTestComplete <*> map mkComplete (take 3 tenPowers)

