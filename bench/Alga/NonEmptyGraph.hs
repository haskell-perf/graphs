{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Alga.NonEmptyGraph
  (
  functions
  )
where

import BenchGraph
import BenchGraph.Utils

import Algebra.Graph.NonEmpty

import qualified Data.List.NonEmpty as NE

instance GraphImpl (NonEmptyGraph Int) where
  mkGraph = edges1 . NE.fromList

vertexList' :: Suite (NonEmptyGraph Int)
vertexList' = simpleSuite "vertexList" vertexList1

edgeList' :: Suite (NonEmptyGraph Int)
edgeList' = simpleSuite "edgeList" edgeList

--A simple function
hasEdge' :: Suite (NonEmptyGraph Int)
hasEdge' = Suite { suiteName = "hasEdge (not in graph)"
                 , algorithm = uncurry hasEdge
                 , inputs    = withNames . take 2 . edgesNotInGraph }

connect' :: Suite (NonEmptyGraph Int)
connect' = Suite { suiteName = "add a new vertex"
                 , algorithm = connect
                 , inputs    = \x -> [("new vertex: " ++ (show $ getNewV x), vertex $ getNewV x)]}
    where
      getNewV x = 1 + extractMaxVertex x

functions :: [Suite (NonEmptyGraph Int)]
functions = [connect', hasEdge', edgeList', vertexList']
