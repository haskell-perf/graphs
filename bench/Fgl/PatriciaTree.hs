{-# LANGUAGE FlexibleInstances #-}

module Fgl.PatriciaTree
(functions)
where

import BenchGraph
import BenchGraph.GenericGraph (vertices)

import BenchGraph.Utils

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

instance GraphImpl UGr where
  mkGraph e = mkUGraph (vertices e) e

isEmpty' :: Suite UGr
isEmpty' = simpleSuite "isEmpty" isEmpty

edgeList :: Suite UGr
edgeList = simpleSuite "edgeList" edges

vertexList :: Suite UGr
vertexList = simpleSuite "vertexList" nodes

hasEdge' :: Suite UGr
hasEdge' = Suite "hasEdge (not in graph)" (flip hasEdge) $
    withNames . take 2 . edgesNotInGraph

insNode' :: Suite UGr
insNode' = Suite { suiteName = "add a new vertex"
             , algorithm = insNode
                 , inputs    = \x -> [("new vertex: " ++ (show $ getNewV x),(getNewV x,()))] }
    where
      getNewV x = 1 + extractMaxVertex x

functions :: [Suite UGr]
functions = [insNode', hasEdge', isEmpty', edgeList, vertexList]

