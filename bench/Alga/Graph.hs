{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Alga.Graph
  (
  functions
  )
where

import BenchGraph
import BenchGraph.Utils

import Algebra.Graph

instance GraphImpl (Graph Int) where
  mkGraph = edges

-- A simple consummer
isEmpty' :: Suite (Graph Int)
isEmpty' = simpleSuite "isEmpty" isEmpty

vertexList' :: Suite (Graph Int)
vertexList' = simpleSuite "vertexList" vertexList

edgeList' :: Suite (Graph Int)
edgeList' = simpleSuite "edgeList" edgeList

--A simple function
hasEdge' :: Suite (Graph Int)
hasEdge' = Suite { suiteName = "hasEdge (not in graph)"
                 , algorithm = uncurry hasEdge
                 , inputs    = withNames . take 2 . edgesNotInGraph }

connect' :: Suite (Graph Int)
connect' = Suite { suiteName = "add a new vertex"
                 , algorithm = connect
                 , inputs    = \x -> [("new vertex: " ++ (show $ getNewV x), vertex $ getNewV x)]}
    where
      getNewV x = 1 + extractMaxVertex x

functions :: [Suite (Graph Int)]
functions = [connect', hasEdge', isEmpty', edgeList', vertexList']

