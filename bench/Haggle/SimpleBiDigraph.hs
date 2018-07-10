{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Haggle.SimpleBiDigraph
  (functions)
where

import BenchGraph.Types
import BenchGraph.Utils hiding (vertices)
import qualified BenchGraph.Suites as S

import Control.Monad (replicateM, mapM_)
import Control.Monad.ST
import Data.Bifunctor

import Data.Graph.Haggle
import Data.Graph.Haggle.Internal.Basic (Vertex (..))
import Data.Graph.Haggle.Algorithms.DFS (topsort, reachable)

instance GraphImpl SimpleBiDigraph where
  mkGraph e = runST $ do
    g <- newSizedMSimpleBiDigraph maxV undefined
    vert <- replicateM maxV (addVertex g)
    mapM_ (\(u,v) -> addEdge g (vert !! u)(vert !! v)) e
    freeze g
    where
      maxV = extractMaxVertex e + 1
  mkVertex = runST $ newSizedMSimpleBiDigraph 1 0 >>= freeze

functions :: [Suite SimpleBiDigraph]
functions =
  [ S.isEmpty isEmpty
  , S.vertexList vertices
  , S.edgeList edges
  , S.hasEdge (\(u,v) g -> edgeExists g u v) (bimap V V)
  , S.topSort topsort
  , S.reachable reachable V
  ]
