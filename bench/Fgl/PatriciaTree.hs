{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Fgl.PatriciaTree
  (functions, mk)
where

import BenchGraph.Suites (SuiteWithExp)
import BenchGraph.Types
import BenchGraph.GenericGraph (Edges)
import BenchGraph.Utils
import qualified BenchGraph.Suites as S

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS

import Data.Maybe (isJust)

-- $setup
-- >>> import BenchGraph.GenericGraph
-- >>> let path10 = mk $ fst $ snd path 1

instance GraphImpl UGr where
  mkGraph = mk
  mkVertex = mkUGraph [0] []

mk :: Edges -> UGr
mk e = mkUGraph (vertices e) e

functions :: [SuiteWithExp UGr]
functions =
  [ Right $ S.isEmpty isEmpty
  , Right $ S.edgeList edges
  , Right $ S.edgeCount size
  , Right $ S.vertexCount order
  , Right $ S.vertexList nodes
  , Right $ S.hasVertex (\x y -> isJust $ lab y x) id -- lab is the only function not erroring if tested with a vertex not in the graph
  , Right $ S.hasEdge (flip hasEdge) id
  , Right $ S.addVertex insNode (\x -> (x,()))
  , Right $ S.removeVertex delNode id
  , Right $ S.eq equal
  , Right $ S.addEdge insEdge (\(x,y) -> (x,y,()))
  , Right $ S.removeEdge delEdge id
  , Right $ S.context (&) (\(x,y) -> ([],x,(),[((),y)]))
  , Right $ S.dff dfs'
  , Right $ S.topSort topsort
  , Right $ S.reachable reachable id
  , Right $ S.transpose transpose
  ]

-- |
-- >>> hasEdge (transpose path10) (1,0)
-- True
--
-- >>> hasEdge (transpose path10) (0,1)
-- False
transpose :: UGr -> UGr
transpose = gmap flip4
  where
    flip4 (a,b,c,d) = (d,b,c,a)
