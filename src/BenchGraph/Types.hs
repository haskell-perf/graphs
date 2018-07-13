{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}

module BenchGraph.Types (
  ShadowedS (..),
  ShadowedSIO (..),
  Suite (..),
  simpleSuite,
  SuiteIO (..),
  simpleSuiteIO,
  GraphImpl (..),
  extractDescription,
  name
) where

import Control.DeepSeq (NFData)

import BenchGraph.GenericGraph
import BenchGraph.Named

-- | Type to shadow the argument of a Suite
data ShadowedS = forall g. (GraphImpl g, NFData g) => Shadow (Suite g)
data ShadowedSIO = forall g. (GraphImpl (IO g), NFData g) => ShadowIO (SuiteIO g)

-- | A graph algorithm operates on a graph type @g@, which takes an input of
-- type @i@ and produces an output of type @o@. Algorithms come with a list of
-- named inputs, all of which will be tried during benchmarking.
data Suite g = forall i o. NFData o => Suite
  { nameN :: String
  , descN :: String
  , algorithmN :: i -> g -> o
  , inputsN    :: Edges -> [Named i] }

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

class HaveDesc a where
  extractDescription :: a -> Named String
  name :: a -> String
  name = fst . extractDescription

instance HaveDesc (Suite a) where
  extractDescription (Suite name' desc' _ _) = (name',desc')

instance HaveDesc (SuiteIO a) where
  extractDescription (SuiteIO name' desc' _ _) = (name',desc')

instance HaveDesc ShadowedS where
  extractDescription (Shadow a) = extractDescription a

instance HaveDesc ShadowedSIO where
  extractDescription (ShadowIO a) = extractDescription a

instance HaveDesc (Either ShadowedS ShadowedSIO) where
  extractDescription = either extractDescription extractDescription
