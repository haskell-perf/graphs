{-# LANGUAGE FlexibleInstances #-}

module HashGraph.Gr
(functions)
where

import BenchGraph
import BenchGraph.GenericGraph (vertices)

import BenchGraph.Utils

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
hasEdge' = Suite "hasEdge (not in graph)" HG.hasEdge $
    map (\(x,y)->(x,mkEdge y)) . withNames . take 2 . edgesNotInGraph

insNode' :: Suite Gr
insNode' = Suite { suiteName = "add a new vertex"
             , algorithm = HG.insNode
             , inputs    = \x -> [("new vertex: " ++ (show $ getNewV x),getNewV x)] }
    where
      getNewV x = 1 + extractMaxVertex x

functions :: [Suite Gr]
functions = [insNode', hasEdge', isEmpty', edgeList, vertexList]

