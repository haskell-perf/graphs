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

isEmpty' :: Suite UGr
isEmpty' = simpleSuite "isEmpty" isEmpty

edgeList :: Suite UGr
edgeList = simpleSuite "edgeList" edges

vertexList :: Suite UGr
vertexList = simpleSuite "vertexList" nodes

hasEdge' :: Suite UGr
hasEdge' = Suite "hasEdge (not in graph)" (flip hasEdge) $
    withNames . take 2 . edgesNotInGraph

allBenchs :: [Benchmark]
allBenchs = map (benchmark graphs) generics
  where
    generics = [hasEdge', isEmpty', edgeList, vertexList]

