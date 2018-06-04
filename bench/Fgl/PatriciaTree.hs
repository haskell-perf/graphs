{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Fgl.PatriciaTree
  (functions, mk, chromaticPolynomial)
where

import BenchGraph
import BenchGraph.GenericGraph (Edges)
import BenchGraph.Utils
import qualified BenchGraph.Suites as S

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS

import Data.List (uncons)
import Data.Tuple (swap)
import Data.Maybe (isJust)
import Common

instance GraphImpl UGr where
  mkGraph = mk
  mkVertex = mkUGraph [0] []

mk :: Edges -> UGr
mk e = mkUGraph (vertices e) e

functions :: [Suite UGr]
functions =
  [ S.isEmpty isEmpty
  , S.edgeList edges
  , S.edgeCount size
  , S.vertexCount order
  , S.vertexList nodes
  , S.hasVertex (\x y -> isJust $ lab y x) id -- lab is the only function not erroring if tested with a vertex not in the graph
  , S.hasEdge (flip hasEdge) id
  , S.addVertex insNode (\x -> (x,()))
  , S.removeVertex delNode id
  , S.eq equal
  , S.addEdge insEdge (\(x,y) -> (x,y,()))
  , S.removeEdge delEdge id
  , S.context (&) (\(x,y) -> ([],x,(),[((),y)]))
  , S.dff dfs'
  , S.topSort topsort
  , S.reachable reachable id
  ]

-- | The graph MUST be undirected (ie if (1,2) is in the graph, then (2,1) is)
chromaticPolynomial :: Gr a b -> [Int]
chromaticPolynomial gr = case getEdge of
  Nothing -> case order gr of
               0 -> [0]
               el -> replicate el 0 ++ [1]
  Just e -> substractPoly (chromaticPolynomial (deleted e)) (chromaticPolynomial (contracted e))
  where
    getEdge = uncons (edges gr) >>= Just . fst
    deleted e = delEdge (swap e) $ delEdge e gr
    contracted (x1,y1) = insEdges (map (\(l,n) -> (n,y1,l)) $ filter ((/=) y1 . snd) prev) $ insEdges (map (\(l,n) -> (y1,n,l)) $ filter ((/=) y1 . snd) next) ngr
      where
        (Just (prev,_,_,next), ngr) = match x1 gr
