{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Containers.Graph
(functions)
where

import BenchGraph
import BenchGraph.Utils (extractMaxVertex)
import BenchGraph.Suites

import Data.Graph

-- For example with alga
instance GraphImpl Graph where
  mkGraph e = buildG (0,extractMaxVertex e) e

functions :: [Suite Graph]
functions =
  [ edgeListS edges
  , vertexListS vertices
  , eqS (==)
  ]
