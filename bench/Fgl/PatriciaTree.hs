{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Fgl.PatriciaTree
  (functions, mk)
where

import BenchGraph.Types
import BenchGraph.GenericGraph (Edges)
import BenchGraph.Utils

import BenchGraph.Suites (a,s)
import qualified BenchGraph.Suites as S

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS

import Data.Maybe (isJust)

instance GraphImpl UGr where
  mkGraph = mk
  mkVertex = mkUGraph [0] []

mk :: Edges -> UGr
mk e = mkUGraph (vertices e) e

functions :: [Suite UGr]
functions =
  [ s S.isEmpty isEmpty
  , s S.edgeList edges
  , s S.edgeCount size
  , s S.vertexCount order
  , s S.vertexList nodes
  , a S.hasVertex (\x y -> isJust $ lab y x) id -- lab is the only function not erroring if tested with a vertex not in the graph
  , a S.hasEdge (flip hasEdge) id
  , a S.addVertex insNode (\x -> (x,()))
  , a S.removeVertex delNode id
  , S.eq equal
  , a S.addEdge insEdge (\(x,y) -> (x,y,()))
  , a S.removeEdge delEdge id
  , a S.context (&) (\(x,y) -> ([],x,(),[((),y)]))
  , s S.dff dfs'
  , s S.topSort topsort
  , a S.reachable reachable id
  ]

