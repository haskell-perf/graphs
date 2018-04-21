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

edgeList :: Suite Graph
edgeList = simpleSuite "edgeList" edges

vertexList :: Suite Graph
vertexList = simpleSuite "vertexList" vertices

allBenchs :: [Benchmark]
allBenchs = map (benchmark graphs) toTest
  where
    toTest = [edgeList, vertexList]

