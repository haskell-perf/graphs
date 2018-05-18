{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Containers.Graph
  (functions, mk)
where

import BenchGraph
import BenchGraph.Utils (extractMaxVertex)
import BenchGraph.Suites
import BenchGraph.GenericGraph (Edges)

import Data.Graph

-- For example with alga
instance GraphImpl Graph where
  mkGraph = mk

mk :: Edges -> Graph
mk e = buildG (0,extractMaxVertex e) e

functions :: [Suite Graph]
functions =
  [ edgeListS edges
  , vertexListS vertices
  , eqS (==)
  , transposeS transposeG
  ]
