{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Fgl.PatriciaTree
  (functions, mk)
where

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

functions :: [Suite UGr]
functions =
  [ S.isEmpty isEmpty
  , S.edgeList edges
  , S.edgeCount size
  , S.vertexCount order
  , S.vertexList nodes
  , S.hasVertex (\x y -> isJust $ lab y x) id -- lab is the only function not erroring if tested with a vertex not in the graph
  , S.hasEdge (flip hasEdge) id
  , S.hasSelfLoop (\x -> flip hasEdge (x,x)) id
  , S.addVertex insNode (\x -> (x,()))
  , S.removeVertex delNode id
  , S.eq equal
  , S.addEdge insEdge (\(x,y) -> (x,y,()))
  , S.removeEdge delEdge id
  , S.context (&) (\(x,y) -> ([],x,(),[((),y)]))
  , S.dff dfs'
  , S.topSort topsort
  , S.reachable reachable id
  , S.transpose transpose
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
