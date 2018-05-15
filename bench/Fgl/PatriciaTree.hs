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


functions :: [Suite UGr]
functions =
  [ isEmptyS isEmpty
  , edgeListS edges
  , vertexListS nodes
  , hasEdgeS (flip hasEdge) id
  , addVertexS insNode (\x -> (x,()))
  , removeVertexS delNode id
  , eqS (==)
  , addEdgeS insEdge (\(x,y) -> (x,y,()))
  , removeEdgeS delEdge id
  ]

