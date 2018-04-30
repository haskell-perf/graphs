{-# LANGUAGE FlexibleInstances #-}

module Fgl.PatriciaTree
(functions)
where

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

functions :: [Suite UGr]
functions = [hasEdge', isEmpty', edgeList, vertexList]

