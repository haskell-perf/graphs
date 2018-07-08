{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Alga.Graph
  (functions, mk, mkSpecClique)
where

import BenchGraph.Types
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
  [ S.vertexList vertexList
  , S.isEmpty isEmpty
  , S.vertexCount vertexCount
  , S.hasVertex hasVertex id
  , S.edgeCount edgeCount
  , S.edgeList edgeList
  , S.hasEdge (uncurry hasEdge) id
  , S.hasSelfLoop hasSelfLoop id
  , S.addEdge (\(x,y) -> overlay $ edge x y) id
  , S.addVertex connect vertex
  , S.removeVertex removeVertex id
  , S.eq (==)
  , S.removeEdge (uncurry removeEdge) id
  , S.transpose transpose
  ]

