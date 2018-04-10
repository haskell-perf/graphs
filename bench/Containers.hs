{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative ((<*>))

import Criterion.Main

import BenchGraph
import BenchGraph.Path

import Data.Graph

-- For example with alga
instance GraphImpl Graph where
  mkGraph = \e -> buildG (0,extractMaxVertex e) e

-- A simple consummer
edges' :: ToFuncToBench Graph
edges' = const $ Consummer "edges" edges

tenPowers :: [Int]
tenPowers = 1: map (10*) tenPowers

main :: IO ()
main = do
  let toTest = map benchFunc [edges']
  defaultMain $ toTest <*> (map mkPath $ take 5 tenPowers)
