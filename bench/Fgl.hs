{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative ((<*>))

import Criterion.Main

import BenchGraph
import BenchGraph.Path
import BenchGraph.GenericGraph (vertices)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

-- For example with alga
instance GraphImpl (UGr) where
  mkGraph = \e -> mkUGraph (vertices e) e

isEmpty' :: ToFuncToBench (UGr)
isEmpty' = const $ Consummer "IsEmpty" isEmpty

pathHasEdge :: ToFuncToBench (UGr)
pathHasEdge = FuncWithArg "hasEdge" (flip hasEdge) show . take 2 . edgesNotInPath

tenPowers :: [Int]
tenPowers = 1: map (10*) tenPowers

main :: IO ()
main = do
  let toTest = map benchFunc [isEmpty',pathHasEdge]
  defaultMain $ toTest <*> (map mkPath $ take 5 tenPowers)
