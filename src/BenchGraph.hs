{-# LANGUAGE ExistentialQuantification #-}

module BenchGraph (
  Suite (..),
  simpleSuite,
  withNames,
  GraphImpl,
  mkGraph,
  benchmark,
  weigh,
  allBenchs,
  allWeighs,
  benchmarkCreation,
  weighCreation,
  weighCreationList,
  computeSize
) where

import Criterion.Main
import Weigh
import GHC.DataSize

import Control.DeepSeq (NFData, ($!!))
import Control.Comonad (extract)
import Control.Monad (when)

import BenchGraph.GenericGraph
import BenchGraph.Utils (graphs)
import BenchGraph.Named

-- A graph algorithm operates on a graph type @g@, which takes an input of
-- type @i@ and produces an output of type @o@. Algorithms come with a list of
-- named inputs, all of which will be tried during benchmarking.
data Suite g = forall i o. NFData o => Suite
    { suiteName :: Name
    , algorithm :: i -> g -> o
    , inputs    :: Edges -> [Named i] }

-- Not the best name, but still better than "consumer", since all algorithms
-- are consumers.
simpleSuite :: NFData o => Name -> (g -> o) -> Suite g
simpleSuite name algorithm = Suite name (const algorithm) (const [Named "" ()])

-- Show items in a list
withNames :: Show a => [a] -> [Named a]
withNames = map nameShow

-- An interface between our generic graphs and others
class GraphImpl g where
    mkGraph :: Edges -> g

---- Criterion
benchmark :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> Suite g -> Benchmark
benchmark graphs (Suite sname algo inputs) = bgroup sname cases
  where
    cases = [ bgroup (show g) $ map (benchSuite algo inputs g) ss | (g, ss) <- graphs ]

benchSuite :: (GraphImpl g, NFData g, NFData o)
           => (i -> g -> o) -> (Edges -> [Named i]) -> GenericGraph -> Size -> Benchmark
benchSuite algorithm inputs g size = bgroup (show size) cases
  where
    edges = extract g size
    graph = mkGraph edges
    cases = [ bench name $ nf (algorithm i) $!! graph | (Named name i) <- inputs edges ]

allBenchs :: (GraphImpl g, NFData g) => (Int,Int,Int) -> [Suite g] -> [Benchmark]
allBenchs size = map (benchmark $ graphs size)

benchmarkCreation :: (NFData g) => (Int,Int,Int) -> (Edges -> g) -> [Benchmark]
benchmarkCreation size mk = [ bgroup ("make a " ++  n ++ " from a list") $ map (\i -> bench (show i) $ nf mk $ grf i ) ss | (Named n grf, ss) <- graphs size ]

---- Weigh
weigh :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> Suite g -> Weigh ()
weigh graphs (Suite sname algo inputs) = wgroup sname cases
  where
    cases = mapM_ (uncurry mkGroup) graphs
    mkGroup g ss = wgroup (show g) $ mapM_ (weighSuite algo inputs g) ss

weighSuite :: (GraphImpl g, NFData g, NFData o)
           => (i -> g -> o) -> (Edges -> [Named i]) -> GenericGraph -> Size -> Weigh ()
weighSuite algorithm inputs g size = wgroup (show size) cases
  where
    edges = extract g size
    graph = mkGraph edges
    cases = mapM_ (uncurry wFunc . fromNamed) $ inputs edges
    wFunc name i = func name (algorithm i) $!! graph

allWeighs :: (GraphImpl g, NFData g) => [Suite g] -> Weigh ()
allWeighs = mapM_ (weigh $ graphs (3,3,2))

-- | Use the list from weighCreationList
weighCreation :: (NFData g)
              => Maybe String -- ^ Maybe a selected bench to do
              -> (Edges -> g) -- ^ A graph-creator function, typically from the GraphImpl class
              -> Weigh ()
weighCreation name mk = sequence_ [when (todo str) $ wgroup str $ mapM_ (\i -> func (show i) mk $ grf i ) ss | Named str (Named n grf, ss) <- weighCreationList ]
  where
    todo str  = maybe True (str ==) name

-- | List of generic graph with their case-name
weighCreationList :: [Named (GenericGraph, [Int])]
weighCreationList = [ Named (str n) t | t@(Named n _, _) <- graphs (3,3,2)]
  where
    str n = "make a " ++ n ++ " from a list"

---- DataSize

computeSize :: (NFData g) => (Int,Int,Int) -> (Edges -> g) -> IO [Named [Named Word]]
computeSize size fun = mapM (\(g,ss) -> sequence $ Named (show g) $ mapM (\s -> sequence $ Named (show s) $ recursiveSize $!! fun $ extract g s) ss) $ graphs size

