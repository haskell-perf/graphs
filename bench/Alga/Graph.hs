{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Alga.Graph
  (
  functions
  )
where

import BenchGraph
import BenchGraph.Suites

import Algebra.Graph

instance GraphImpl (Graph Int) where
  mkGraph = edges

-- A simple consumer
isEmpty' :: Suite (Graph Int)
isEmpty' = isEmptyS isEmpty

vertexList' :: Suite (Graph Int)
vertexList' = isEmptyS vertexList

edgeList' :: Suite (Graph Int)
edgeList' = isEmptyS edgeList

--A simple function
hasEdge' :: Suite (Graph Int)
hasEdge' = hasEdgeS (uncurry hasEdge) id

connect' :: Suite (Graph Int)
connect' = addVertexS connect vertex

removeVertex' :: Suite (Graph Int)
removeVertex' = removeVertexS removeVertex id

eq :: Suite (Graph Int)
eq = eqS (==)

removeEdge' :: Suite (Graph Int)
removeEdge' = removeEdgeS (uncurry removeEdge) id

functions :: [Suite (Graph Int)]
functions = [connect', removeVertex', hasEdge', isEmpty', edgeList', vertexList', eq, removeEdge']

