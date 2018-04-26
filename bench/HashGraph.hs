{-# LANGUAGE FlexibleInstances #-}

module HashGraph 
(allBenchs)
where

import Criterion.Main

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

allBenchs :: [Benchmark]
allBenchs = map (benchmark graphs) generics
  where
    generics = [hasEdge', isEmpty', edgeList, vertexList]

