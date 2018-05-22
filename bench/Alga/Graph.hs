{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Alga.Graph
  (functions, mk)
where

import BenchGraph
import qualified BenchGraph.Suites as S
import BenchGraph.GenericGraph (Edges)

import Algebra.Graph

instance GraphImpl (Graph Int) where
  mkGraph = edges

mk :: Edges -> Graph Int
mk = edges

functions :: [Suite (Graph Int)]
functions =
  [ S.isEmpty isEmpty
  , S.vertexList vertexList
  , S.edgeList edgeList
  , S.hasEdge (uncurry hasEdge) id
  , S.addEdge (\(x,y) -> overlay $ edge x y) id
  , S.addVertex connect vertex
  , S.removeVertex removeVertex id
  , S.eq (==)
  , S.removeEdge (uncurry removeEdge) id
  , S.transpose transpose
  ]

