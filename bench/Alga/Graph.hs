{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Alga.Graph
  (functions, mk, mkSpecClique)
where

import BenchGraph.Types
import qualified BenchGraph.Suites as S
import BenchGraph.GenericGraph (Edges)
import BenchGraph.Utils (extractMaxVertex)

import Algebra.Graph
import qualified Algebra.Graph.AdjacencyIntMap as AIM
import qualified Algebra.Graph.AdjacencyIntMap.Algorithm as AIM
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM

instance GraphImpl (Graph Int) where
  mkGraph = mk
  mkVertex = vertex 0

mk :: Edges -> Graph Int
mk = edges

mkSpecClique :: Edges -> Graph Int
mkSpecClique edgs = clique [0..(extractMaxVertex edgs)]

functions :: [SuiteWithExp (Graph Int)]
functions =
  [ Right $ S.vertexList vertexList
  , Right $ S.isEmpty isEmpty
  , Right $ S.vertexCount vertexCount
  , Right $ S.hasVertex hasVertex id
  , Right $ S.edgeCount edgeCount
  , Right $ S.edgeList edgeList
  , Right $ S.hasEdge (uncurry hasEdge) id
  , Right $ S.addEdge (\(x,y) -> overlay $ edge x y) id
  , Right $ S.addVertex connect vertex
  , Right $ S.removeVertex removeVertex id
  , Right $ S.eq (==)
  , Right $ S.removeEdge (uncurry removeEdge) id
  , Right $ S.transpose transpose
  , Right $ S.dff (AIM.dfsForest . toAIM)
  , Right $ S.topSort (AIM.topSort . toAIM)
  , Right $ S.scc (AM.scc . toAM)
  , Right $ S.reachable (\x g -> AIM.reachable x $ toAIM g) id
  , Right $ S.bfs (\x g -> AIM.bfs [x] $ toAIM g) id
  , Left    ("mergeContext","it is a nonsense")
  ]

toAIM :: Graph Int -> AIM.AdjacencyIntMap
toAIM = foldg AIM.empty AIM.vertex AIM.overlay AIM.connect

toAM :: Graph Int -> AM.AdjacencyMap Int
toAM = foldg AM.empty AM.vertex AM.overlay AM.connect
