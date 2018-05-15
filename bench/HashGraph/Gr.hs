{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module HashGraph.Gr
(functions)
where

import BenchGraph
import BenchGraph.GenericGraph (vertices)
import BenchGraph.Suites

import qualified Data.HashGraph.Strict as HG

type Gr = HG.Gr () Int

instance GraphImpl Gr where
  mkGraph e = HG.mkGraph (map mkEdge e) (vertices e)

mkEdge :: (Int,Int) -> HG.Edge () Int
mkEdge (x,y) = HG.Edge x () y

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
  ]
