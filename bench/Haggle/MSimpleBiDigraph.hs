{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Haggle.MSimpleBiDigraph
  (functions)
where

import BenchGraph.Types
import BenchGraph.Utils hiding (vertices)

import BenchGraph.Suites (sIO, aIO)
import qualified BenchGraph.Suites as S

import Control.Monad (replicateM, mapM_)
import Data.Bifunctor

import Data.Graph.Haggle
import Data.Graph.Haggle.Internal.Basic (Vertex (..))

import Control.DeepSeq (NFData (..))

instance GraphImpl (IO (MSimpleBiDigraph IO)) where
  mkGraph e = do
    g <- newSizedMSimpleBiDigraph maxV undefined
    vert <- replicateM maxV (addVertex g)
    mapM_ (\(u,v) -> addEdge g (vert !! u)(vert !! v)) e
    return g
    where
      maxV = extractMaxVertex e + 1
  mkVertex = newSizedMSimpleBiDigraph 1 0

instance NFData (MSimpleBiDigraph IO) where
  rnf = const () -- TODO remove this, as it is not used nor with sense

functions :: [SuiteIO (MSimpleBiDigraph IO)]
functions =
  [ sIO S.vertexCount countVertices
  , sIO S.edgeCount countEdges
  , aIO S.addEdge (\(v1,v2) g -> addEdge g v1 v2) (bimap V V)
--  , simpleSuiteIO (fst' $ S.addVertex undefined) (snd' $ S.addVertex undefined) addVertex
  ]
  where
    fst' (a,_,_) = a
    snd' (_,a,_) = a
