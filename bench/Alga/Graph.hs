{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Alga.Graph
  (functions, mk, mkSpecClique)
where

import BenchGraph.Types
import BenchGraph.Suites (s,a)
import qualified BenchGraph.Suites as S
import BenchGraph.GenericGraph (Edges)
import BenchGraph.Utils (extractMaxVertex)

import Algebra.Graph

instance GraphImpl (Graph Int) where
  mkGraph = mk
  mkVertex = vertex 0

mk :: Edges -> Graph Int
mk = edges

mkSpecClique :: Edges -> Graph Int
mkSpecClique edgs = clique [0..(extractMaxVertex edgs)]

functions :: [Suite (Graph Int)]
functions =
  [ s S.isEmpty isEmpty
  , s S.vertexList vertexList
  , s S.vertexCount vertexCount
  , a S.hasVertex hasVertex id
  , s S.edgeCount edgeCount
  , s S.edgeList edgeList
  , a S.hasEdge (uncurry hasEdge) id
  , a S.hasSelfLoop hasSelfLoop id
  , a S.addEdge (\(x,y) -> overlay $ edge x y) id
  , a S.addVertex connect vertex
  , a S.removeVertex removeVertex id
  , S.eq (==)
  , a S.removeEdge (uncurry removeEdge) id
  , s S.transpose transpose
  ]

