{-# LANGUAGE FlexibleInstances #-}

module Containers
(allBenchs)
where

import Criterion.Main

import BenchGraph
import BenchGraph.Path

import BenchGraph.Utils

import Data.Graph hiding (path)

-- For example with alga
instance GraphImpl Graph where
  mkGraph e = buildG (0,extractMaxVertex e) e

edgeList :: ToFuncToBench Graph
edgeList = const $ Consumer "edgeList" edges

vertexList :: ToFuncToBench Graph
vertexList = const $ Consumer "vertexList" vertices

allBenchs :: [Benchmark]
allBenchs = benchOver path toTest $ take 5 tenPowers
  where
    toTest = [edgeList, vertexList]

