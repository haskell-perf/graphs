{-# LANGUAGE FlexibleInstances #-}

module Containers.Graph
(functions)
where

import BenchGraph

import BenchGraph.Utils

import Data.Graph

-- For example with alga
instance GraphImpl Graph where
  mkGraph e = buildG (0,extractMaxVertex e) e

edgeList :: Suite Graph
edgeList = simpleSuite "edgeList" edges

vertexList :: Suite Graph
vertexList = simpleSuite "vertexList" vertices

functions :: [Suite Graph]
functions = [edgeList, vertexList]

