{-# LANGUAGE FlexibleInstances #-}

module Fgl
(allBenchs)
where

import Control.Applicative ((<*>))

import Criterion.Main

import BenchGraph
import BenchGraph.Path
import BenchGraph.GenericGraph (vertices)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

instance GraphImpl UGr where
  mkGraph e = mkUGraph (vertices e) e

isEmpty' :: ToFuncToBench UGr
isEmpty' = const $ Consummer "IsEmpty" isEmpty

pathHasEdge :: ToFuncToBench UGr
pathHasEdge = FuncWithArg "hasEdge" (flip hasEdge) show . take 2 . edgesNotInPath

tenPowers :: [Int]
tenPowers = 1: map (10*) tenPowers

allBenchs :: [Benchmark]
allBenchs = toTest <*> map mkPath (take 5 tenPowers)
  where
    toTest = map benchFunc [isEmpty',pathHasEdge]

