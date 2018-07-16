{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module HashGraph.Gr
  (functions, mk)
where

import BenchGraph.Types
import BenchGraph.GenericGraph (Edges)
import BenchGraph.Utils
import qualified BenchGraph.Suites as S

import qualified Data.HashGraph.Strict as HG
import qualified Data.HashGraph.Algorithms as A
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as M

-- $setup
-- >>> import BenchGraph.GenericGraph
-- >>> let path10 = mk $ fst $ snd path 1

type Gr = HG.Gr () Int

instance GraphImpl Gr where
  mkGraph = mk
  mkVertex = HG.mkGraph [] [0]

mkEdge :: (Int,Int) -> HG.Edge () Int
mkEdge (x,y) = HG.Edge x () y

mk :: Edges -> Gr
mk e = HG.mkGraph (map mkEdge e) (vertices e)

functions :: [Suite Gr]
functions =
  [ S.isEmpty HG.null
  , S.edgeList HG.edges
  , S.edgeCount HG.size
  , S.vertexCount HG.order
  , S.vertexList HG.nodes
  , S.hasVertex HG.member id
  , S.hasEdge HG.hasEdge mkEdge
  , S.hasSelfLoop HG.hasEdge (\x -> mkEdge (x,x))
  , S.addVertex HG.insNode id
  , S.removeVertex HG.delNode id
  , S.eq (==)
  , S.addEdge HG.insEdge mkEdge
  , S.removeEdge HG.delEdge mkEdge
  , S.context (HG.&) $ \(x,y) -> (x,HG.Context' Set.empty (Set.singleton (HG.Tail () y)))
  , S.dff A.dfs
  , S.topSort A.topSort
  , S.transpose transpose
  ]

-- |
-- >>> HG.hasEdge (mkEdge (1,0)) $ transpose path10
-- True
--
-- >>> HG.hasEdge (mkEdge (0,1)) $ transpose path10
-- False
transpose :: Gr -> Gr
transpose (HG.Gr g) = HG.Gr $ M.map (\(HG.Context' h t) -> HG.Context' (Set.map (\(HG.Tail a b) -> HG.Head a b) t) (Set.map (\(HG.Head a b) -> HG.Tail a b) h)) g
