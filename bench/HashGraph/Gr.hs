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

isEmpty' :: Suite Gr
isEmpty' = isEmptyS HG.null

edgeList :: Suite Gr
edgeList = edgeListS HG.edges

vertexList :: Suite Gr
vertexList = vertexListS HG.nodes

hasEdge' :: Suite Gr
hasEdge' = hasEdgeS HG.hasEdge mkEdge

insNode' :: Suite Gr
insNode' = addVertexS HG.insNode id

removeVertex' :: Suite Gr
removeVertex' = removeVertexS HG.delNode id

eq :: Suite Gr
eq = eqS (==)

functions :: [Suite Gr]
functions = [removeVertex', insNode', hasEdge', isEmpty', edgeList, vertexList, eq]

