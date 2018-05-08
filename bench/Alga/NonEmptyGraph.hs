{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Alga.NonEmptyGraph
  (
  functions
  )
where

import BenchGraph
import BenchGraph.Suites

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
hasEdge' = hasEdgeS (uncurry hasEdge) id

connect' :: Suite (NonEmptyGraph Int)
connect' = addVertexS connect vertex

removeVertex' :: Suite (NonEmptyGraph Int)
removeVertex' = removeVertexS removeVertex1 id

eq :: Suite (NonEmptyGraph Int)
eq = eqS (==)

functions :: [Suite (NonEmptyGraph Int)]
functions = [removeVertex', connect', hasEdge', edgeList', vertexList', eq]
