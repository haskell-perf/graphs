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

functions :: [Suite Graph]
functions =
  [ S.edgeList edges
  , S.vertexList vertices
  , S.eq (==)
  , S.transpose transposeG
  , S.dff dff
  , S.topSort topSort
  , S.reachable (flip reachable) id
  , S.edgeCount edgeCount
  , S.hasEdge hasEdge id
  , S.isEmpty null
  , S.vertexCount vertexCount
  , S.hasVertex hasVertex id
  , S.addVertex addVertex id
  , S.addEdge addEdge id
  , S.removeEdge removeEdge id
  , S.hasSelfLoop (\x -> hasEdge (x,x)) id
  -- removeVertex is not implementable
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
-- >>> vertexCount $ addVertex 10 path10
-- 11
--
-- >>> vertexCount $ addVertex (-1) path10
-- 11
--
-- >>> hasVertex 15 $ addVertex 15 fiveVertices
-- True
addVertex :: Int -> Graph -> Graph
addVertex i g =
  if i >= f
     then if i <= l
             then g
             else listArray (f,i) $ edgeList ++ diff l
     else listArray (i,l) $ diff f ++ edgeList
  where
    edgeList = elems g
    (f,l) = bounds g
    diff k = replicate (abs (k-i)) []

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
