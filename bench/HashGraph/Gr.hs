{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module HashGraph.Gr
  (functions, mk)
where

import BenchGraph
import BenchGraph.GenericGraph (Edges,vertices)
import BenchGraph.Suites

import qualified Data.HashGraph.Strict as HG
import qualified Data.HashSet as S

type Gr = HG.Gr () Int

instance GraphImpl Gr where
  mkGraph = mk

mkEdge :: (Int,Int) -> HG.Edge () Int
mkEdge (x,y) = HG.Edge x () y

mk :: Edges -> Gr
mk e = HG.mkGraph (map mkEdge e) (vertices e)

functions :: [Suite Gr]
functions =
  [ isEmptyS HG.null
  , edgeListS HG.edges
  , vertexListS HG.nodes
  , hasEdgeS HG.hasEdge mkEdge
  , addVertexS HG.insNode id
  , removeVertexS HG.delNode id
  , eqS (==)
  , addEdgeS HG.insEdge mkEdge
  , removeEdgeS HG.delEdge mkEdge
  , contextS (HG.&) $ \(x,y) -> (x,HG.Context' S.empty (S.singleton (HG.Tail () y)))
  ]
