{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module HashGraph.Gr
  (functions, mk)
where

import BenchGraph
import BenchGraph.GenericGraph (Edges)
import BenchGraph.Utils
import qualified BenchGraph.Suites as S

import qualified Data.HashGraph.Strict as HG
import qualified Data.HashSet as Set

type Gr = HG.Gr () Int

instance GraphImpl Gr where
  mkGraph = mk

mkEdge :: (Int,Int) -> HG.Edge () Int
mkEdge (x,y) = HG.Edge x () y

mk :: Edges -> Gr
mk e = HG.mkGraph (map mkEdge e) (vertices e)

functions :: [Suite Gr]
functions =
  [ S.isEmpty HG.null
  , S.edgeList HG.edges
  , S.edgeCount HG.size
  , S.vertexCount HG.order
  , S.vertexList HG.nodes
  , S.hasEdge HG.hasEdge mkEdge
  , S.addVertex HG.insNode id
  , S.removeVertex HG.delNode id
  , S.eq (==)
  , S.addEdge HG.insEdge mkEdge
  , S.removeEdge HG.delEdge mkEdge
  , S.context (HG.&) $ \(x,y) -> (x,HG.Context' Set.empty (Set.singleton (HG.Tail () y)))
  ]
