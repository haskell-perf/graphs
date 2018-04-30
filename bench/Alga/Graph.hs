{-# LANGUAGE FlexibleInstances #-}

module Alga.Graph
  (
  functions
  )
where

import BenchGraph
import BenchGraph.Utils

import Algebra.Graph

instance GraphImpl (Graph Int) where
  mkGraph = edges

-- A simple consummer
isEmpty' :: Suite (Graph Int)
isEmpty' = simpleSuite "isEmpty" isEmpty

vertexList' :: Suite (Graph Int)
vertexList' = simpleSuite "vertexList" vertexList

edgeList' :: Suite (Graph Int)
edgeList' = simpleSuite "edgeList" edgeList

--A simple function
hasEdge' :: Suite (Graph Int)
hasEdge' = Suite { suiteName = "hasEdge (not in graph)"
                 , algorithm = uncurry hasEdge
                 , inputs    = withNames . take 2 . edgesNotInGraph }

functions :: [Suite (Graph Int)]
functions = [hasEdge', isEmpty', edgeList', vertexList']

