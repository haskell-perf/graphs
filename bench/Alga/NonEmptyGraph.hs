{-# LANGUAGE FlexibleInstances #-}

module Alga.NonEmptyGraph
(allBenchs)
where

import Criterion.Main

import BenchGraph

import BenchGraph.Utils

import Algebra.Graph.NonEmpty

import qualified Data.List.NonEmpty as NE

instance GraphImpl (NonEmptyGraph Int) where
  mkGraph = edges1 . NE.fromList

vertexList' :: Suite (NonEmptyGraph Int)
vertexList' = simpleSuite "vertexList" vertexList1

edgeList' :: Suite (NonEmptyGraph Int)
edgeList' = simpleSuite "edgeList" edgeList

--A simple function
hasEdge' :: Suite (NonEmptyGraph Int)
hasEdge' = Suite { suiteName = "hasEdge (not in graph)"
                 , algorithm = uncurry hasEdge
                 , inputs    = withNames . take 2 . edgesNotInGraph }

allBenchs :: [Benchmark]
allBenchs = toTest
  where
    generics = [hasEdge', edgeList', vertexList']

    toTest = map (benchmark graphs) generics
