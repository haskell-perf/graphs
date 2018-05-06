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
isEmpty' = simpleSuite "isEmpty" HG.null

edgeList :: Suite Gr
edgeList = simpleSuite "edgeList" HG.edges

vertexList :: Suite Gr
vertexList = simpleSuite "vertexList" HG.nodes

hasEdge' :: Suite Gr
hasEdge' = hasEdgeS HG.hasEdge (\(x,y)->(x,mkEdge y))

insNode' :: Suite Gr
insNode' = addVertexS HG.insNode id

removeVertex' :: Suite Gr
removeVertex' = removeVertexS HG.delNode id

functions :: [Suite Gr]
functions = [removeVertex', insNode', hasEdge', isEmpty', edgeList, vertexList]

