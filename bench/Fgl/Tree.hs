{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Fgl.Tree
  (functions, mk)
where

import BenchGraph
import BenchGraph.GenericGraph (Edges,vertices)
import BenchGraph.Suites

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

instance GraphImpl UGr where
  mkGraph = mk

mk :: Edges -> UGr
mk e = mkUGraph (vertices e) e

functions :: [Suite UGr]
functions =
  [ isEmptyS isEmpty
  , edgeListS edges
  , vertexListS nodes
  , hasEdgeS (flip hasEdge) id
  , addVertexS insNode (\x -> (x,()))
  , removeVertexS delNode id
  , eqS equal
  , addEdgeS insEdge (\(x,y) -> (x,y,()))
  , removeEdgeS delEdge id
  ]
