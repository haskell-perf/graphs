{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Fgl.PatriciaTree
(functions)
where

import BenchGraph
import BenchGraph.GenericGraph (vertices)

import BenchGraph.Suites

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

instance GraphImpl UGr where
  mkGraph e = mkUGraph (vertices e) e

isEmpty' :: Suite UGr
isEmpty' = isEmptyS isEmpty

edgeList :: Suite UGr
edgeList = edgeListS edges

vertexList :: Suite UGr
vertexList = vertexListS nodes

--A simple function
hasEdge' :: Suite UGr
hasEdge' = hasEdgeS (flip hasEdge) id

insNode' :: Suite UGr
insNode' = addVertexS insNode (\x -> (x,()))

removeVertex' :: Suite UGr
removeVertex' = removeVertexS delNode id

eq :: Suite UGr
eq = eqS (==)

functions :: [Suite UGr]
functions = [insNode',removeVertex', hasEdge', isEmpty', edgeList, vertexList, eq]

