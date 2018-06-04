{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Alga.Graph
  (functions, mk, chromaticPolynomial)
where

import BenchGraph.Types
import qualified BenchGraph.Suites as S
import BenchGraph.GenericGraph (Edges)

import Common
import Data.List (uncons)

import Algebra.Graph

instance GraphImpl (Graph Int) where
  mkGraph = edges
  mkVertex = vertex 0

mk :: Edges -> Graph Int
mk = edges

functions :: [Suite (Graph Int)]
functions =
  [ S.isEmpty isEmpty
  , S.vertexList vertexList
  , S.vertexCount vertexCount
  , S.hasVertex hasVertex id
  , S.edgeCount edgeCount
  , S.edgeList edgeList
  , S.hasEdge (uncurry hasEdge) id
  , S.addEdge (\(x,y) -> overlay $ edge x y) id
  , S.addVertex connect vertex
  , S.removeVertex removeVertex id
  , S.eq (==)
  , S.removeEdge (uncurry removeEdge) id
  , S.transpose transpose
  ]

-- | The graph MUST be undirected (ie if (1,2) is in the graph, then (2,1) is)
chromaticPolynomial :: Graph Int -> [Int]
chromaticPolynomial gr = case getEdge of
  Nothing -> case vertexCount gr of
               0 -> [0]
               el -> replicate el 0 ++ [1]
  Just e -> substractPoly (chromaticPolynomial (deleted e)) (chromaticPolynomial (contracted e))
  where
    getEdge = uncons (edgeList gr) >>= Just . fst
    deleted (x1,y1) = removeEdge y1 x1 $ removeEdge x1 y1 gr
    contracted (x1,y1) = removeEdge y1 y1 $ replaceVertex x1 y1 gr
