{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module HashGraph.Gr
  (functions, mk)
where

import BenchGraph.Types
import BenchGraph.GenericGraph (Edges)
import BenchGraph.Utils

import BenchGraph.Suites (a,s)
import qualified BenchGraph.Suites as S

import qualified Data.HashGraph.Strict as HG
import qualified Data.HashGraph.Algorithms as A
import qualified Data.HashSet as Set

type Gr = HG.Gr () Int

instance GraphImpl Gr where
  mkGraph = mk
  mkVertex = HG.mkGraph [] [0]

mkEdge :: (Int,Int) -> HG.Edge () Int
mkEdge (x,y) = HG.Edge x () y

mk :: Edges -> Gr
mk e = HG.mkGraph (map mkEdge e) (vertices e)

functions :: [Suite Gr]
functions =
  [ s S.isEmpty HG.null
  , s S.edgeList HG.edges
  , s S.edgeCount HG.size
  , s S.vertexCount HG.order
  , s S.vertexList HG.nodes
  , a S.hasVertex HG.member id
  , a S.hasEdge HG.hasEdge mkEdge
  , a S.addVertex HG.insNode id
  , a S.removeVertex HG.delNode id
  , S.eq (==)
  , a S.addEdge HG.insEdge mkEdge
  , a S.removeEdge HG.delEdge mkEdge
  , a S.context (HG.&) $ \(x,y) -> (x,HG.Context' Set.empty (Set.singleton (HG.Tail () y)))
  , s S.dff A.dfs
  , s S.topSort A.topSort
  ]

