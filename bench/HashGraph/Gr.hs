{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module HashGraph.Gr
  (functions, mk, chromaticPolynomial)
where

import BenchGraph
import BenchGraph.GenericGraph (Edges)
import BenchGraph.Utils
import qualified BenchGraph.Suites as S

import qualified Data.HashGraph.Strict as HG
import qualified Data.HashGraph.Algorithms as A
import qualified Data.HashSet as Set

import Data.List (uncons, foldl')
import Common

type Gr = HG.Gr () Int

instance GraphImpl Gr where
  mkGraph = mk
  mkVertex = HG.mkGraph [] . return

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
  , S.hasEdge HG.hasEdge mkEdge
  , S.addVertex HG.insNode id
  , S.removeVertex HG.delNode id
  , S.eq (==)
  , S.addEdge HG.insEdge mkEdge
  , S.removeEdge HG.delEdge mkEdge
  , S.context (HG.&) $ \(x,y) -> (x,HG.Context' Set.empty (Set.singleton (HG.Tail () y)))
  , S.dff A.dfs
  ]

-- | The graph MUST be undirected (ie if (1,2) is in the graph, then (2,1) is)
chromaticPolynomial :: Gr -> [Int]
chromaticPolynomial gr = case getEdge of
  Nothing -> case HG.order gr of
               0 -> [0]
               el -> replicate el 0 ++ [1]
  Just e -> substractPoly (chromaticPolynomial (deleted e)) (chromaticPolynomial (contracted e))
  where
    getEdge = uncons (HG.edges gr) >>= Just . fst
    deleted e@(HG.Edge x l y) = HG.delEdge (HG.Edge y l x) $ HG.delEdge e gr
    insEdges es g = foldl' (flip HG.insEdge) g es
    contracted (HG.Edge x1 _ y1) = insEdges (map (\(HG.Head l n) -> HG.Edge n l y1) $ filter (\(HG.Head _ x) -> x /= y1) $ Set.toList prev) $ insEdges (map (\(HG.Tail l n) -> HG.Edge y1 l n) $ filter (\(HG.Tail _ x) -> x /= y1) $ Set.toList next) ngr
      where
        Just ((_, HG.Context' prev next), ngr) = HG.match x1 gr
