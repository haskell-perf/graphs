{-# LANGUAGE FlexibleInstances #-}

module Containers
(allBenchs)
where

import Control.Applicative ((<*>))

import Criterion.Main

import BenchGraph
import BenchGraph.Path

import Data.Graph

-- For example with alga
instance GraphImpl Graph where
  mkGraph e = buildG (0,extractMaxVertex e) e

-- A simple consummer
edges' :: ToFuncToBench Graph
edges' = const $ Consummer "edges" edges

tenPowers :: [Int]
tenPowers = 1: map (10*) tenPowers

allBenchs :: [Benchmark]
allBenchs = toTest <*> map mkPath (take 5 tenPowers)
  where
    toTest = map benchFunc [edges']

