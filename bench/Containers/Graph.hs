{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Containers.Graph
  (functions, mk)
where

import BenchGraph.Types
import BenchGraph.Utils (extractMaxVertex)
import qualified BenchGraph.Suites as S
import BenchGraph.GenericGraph (Edges)

import Data.Graph hiding (path)
import Data.Array

-- $setup
-- >>> import BenchGraph.GenericGraph
-- >>> let path10 = mk $ fst $ snd path 1
-- >>> let fiveVertices = buildG (0,4) []

instance GraphImpl Graph where
  mkGraph = mk
  mkVertex = buildG (0,0) []

mk :: Edges -> Graph
mk e = buildG (0,extractMaxVertex e) e

functions :: [SuiteWithExp Graph]
functions =
  [ Right $ S.edgeList edges
  , Right $ S.vertexList vertices
  , Right $ S.eq (==)
  , Right $ S.transpose transposeG
  , Right $ S.dff dff
  , Right $ S.topSort topSort
  , Right $ S.reachable (flip reachable) id
  , Right $ S.edgeCount edgeCount
  , Right $ S.hasEdge hasEdge id
  , Right $ S.isEmpty null
  , Right $ S.vertexCount vertexCount
  , Right $ S.hasVertex hasVertex id
  , Right $ S.addVertex addVertex id
  , Right $ S.addEdge addEdge id
  , Right $ S.removeEdge removeEdge id
  , Right $ S.removeVertex removeVertex id
  , Left    ("mergeContext","it is a nonsense")
  ]

-- |
-- >>> edgeCount path10
-- 9
--
-- >>> edgeCount fiveVertices
-- 0
edgeCount :: Graph -> Int
edgeCount = foldr (\x y -> length x + y) 0

-- |
-- >>> hasEdge (0,1) path10
-- True
--
-- >>> hasEdge (0,2) path10
-- False
--
-- >>> hasEdge (0,1) fiveVertices
-- False
hasEdge :: (Int,Int) -> Graph -> Bool
hasEdge (x,y) g = (x >= u && x <= v) && elem y (g ! x)
  where
    (u,v) = bounds g

-- |
-- >>> vertexCount path10
-- 10
--
-- >>> vertexCount fiveVertices
-- 5
vertexCount :: Graph -> Int
vertexCount g = if v >= u then v - u + 1 else 0
  where
    (u,v) = bounds g

-- |
-- >>> hasVertex 1 path10
-- True
--
-- >>> hasVertex 11 path10
-- False
hasVertex :: Int -> Graph -> Bool
hasVertex i g = (i >= u) && (i <= v)
  where
    (u,v) = bounds g

-- |
--
--  This is a very special 'addVertex' since it add a vertex, but not with the specified indice.
--
-- >>> vertexCount $ addVertex 10 path10
-- 11
--
-- >>> vertexCount $ addVertex (-1) path10
-- 11
addVertex :: Int -> Graph -> Graph
addVertex _ g = listArray (f,l+1) $ edgeList ++ [[]]
  where
    edgeList = elems g
    (f,l) = bounds g

-- |
-- >>> hasEdge (1,0) $ addEdge (1,0) fiveVertices
-- True
--
-- >>> hasEdge (0,1) $ addEdge (1,0) fiveVertices
-- False
addEdge :: (Int,Int) -> Graph -> Graph
addEdge (i,j) g = g // [(i,j:(g ! i))]

-- |
-- >>> hasEdge (0,1) $ removeEdge (0,1) path10
-- False
--
-- >>> hasEdge (0,1) $ removeEdge (1,0) path10
-- True
removeEdge :: (Int,Int) -> Graph -> Graph
removeEdge (i,j) g = g // [(i,filter (/= j) $ g ! i)]

-- |
--
-- This is a very special 'removeVertex' since it remove the desired vertex but relabel the end
-- of the vertices, so there is no gap between indices.
--
-- >>> vertexCount $ removeVertex 1 $ fiveVertices
-- 4
removeVertex :: Int -> Graph -> Graph
removeVertex i g = listArray (f,l-1) $ h ++ tail t
  where
    (h,t) = splitAt (i-f) $ elems g
    (f,l) = bounds g
