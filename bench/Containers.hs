{-# LANGUAGE FlexibleInstances #-}

module Containers
(allBenchs)
where

import Criterion.Main

import BenchGraph

import BenchGraph.Utils

import Data.Graph

-- For example with alga
instance GraphImpl Graph where
  mkGraph e = buildG (0,extractMaxVertex e) e

edgeList :: ToFuncToBench Graph
edgeList = createConsumer "edgeList" edges

vertexList :: ToFuncToBench Graph
vertexList = createConsumer "vertexList" vertices

allBenchs :: [Benchmark]
allBenchs = map (benchOver graphs) toTest 
  where
    toTest = [edgeList, vertexList]

