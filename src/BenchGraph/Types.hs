{-# LANGUAGE ExistentialQuantification #-}

module BenchGraph.Types (
  ShadowedS (..),
  Suite (..),
  simpleSuite,
  SuiteIO (..),
  simpleSuiteIO,
  GraphImpl (..),
  extractDescription
) where

import Control.DeepSeq (NFData)

import BenchGraph.GenericGraph
import BenchGraph.Named

-- | Type to shadow the argument of a Suite
data ShadowedS = forall g. (GraphImpl g, NFData g) => Shadow (Suite g)

-- | A graph algorithm operates on a graph type @g@, which takes an input of
-- type @i@ and produces an output of type @o@. Algorithms come with a list of
-- named inputs, all of which will be tried during benchmarking.
data Suite g = forall i o. NFData o => Suite
  { name :: String
  , desc :: String
  , algorithm :: i -> g -> o
  , inputs    :: Edges -> [Named i] }

-- A suite that don't take arguments apart a graph
simpleSuite :: NFData o => Name -> String -> (g -> o) -> Suite g
simpleSuite name' desc' algorithm' = Suite name' desc' (const algorithm') (const [("",())])

data SuiteIO g = forall i o. NFData o => SuiteIO
  { nameIO :: String
  , descIO :: String
  , algorithmIO :: i -> g -> IO o
  , inputsIO    :: Edges -> [Named i] }

-- A suite that don't take arguments apart a graph
simpleSuiteIO :: NFData o => Name -> String -> (g -> IO o) -> SuiteIO g
simpleSuiteIO name' desc' algorithm' = SuiteIO name' desc' (const algorithm') (const [("",())])

-- An interface between our generic graphs and others
class GraphImpl g where
  mkGraph :: Edges -> g
  mkVertex :: g -- | A single vertex

extractDescription :: Suite a -> Named String
extractDescription (Suite name' desc' _ _) = (name',desc')
