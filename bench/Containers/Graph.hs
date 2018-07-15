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
hasEdge (x,y) g = elem y $ g ! x

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
