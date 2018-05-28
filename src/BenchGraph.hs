{-# LANGUAGE ExistentialQuantification #-}

module BenchGraph (
  Suite (..),
  NSuite,
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
import Criterion.Types (Benchmark (..))
import Weigh
import GHC.DataSize

import Control.DeepSeq (NFData, ($!!))
import Control.Comonad (extract)
import Control.Monad (when)

import BenchGraph.GenericGraph
import BenchGraph.Utils (graphs, defaultGr)
import BenchGraph.Named

-- A graph algorithm operates on a graph type @g@, which takes an input of
-- type @i@ and produces an output of type @o@. Algorithms come with a list of
-- named inputs, all of which will be tried during benchmarking.
data Suite g = forall i o. NFData o => Suite
    { algorithm :: i -> g -> o
    , inputs    :: Edges -> [Named i] }

type NSuite g = Named (Suite g)

-- Not the best name, but still better than "consumer", since all algorithms
-- are consumers.
simpleSuite :: NFData o => Name -> (g -> o) -> NSuite g
simpleSuite name algorithm = Named name $ Suite (const algorithm) (const [Named "" ()])

-- Show items in a list
withNames :: Show a => [a] -> [Named a]
withNames = map nameShow

-- An interface between our generic graphs and others
class GraphImpl g where
    mkGraph :: Edges -> g

---- Criterion
benchmark :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> NSuite g -> Benchmark
benchmark graphs (Named sname (Suite algo inputs)) = bgroup sname cases
  where
    cases = [ bgroup (show g) $ map (benchSuite algo inputs g) ss | (g, ss) <- graphs ]

benchSuite :: (GraphImpl g, NFData g, NFData o)
           => (i -> g -> o) -> (Edges -> [Named i]) -> GenericGraph -> Size -> Benchmark
benchSuite algorithm inputs g size = bgroup (show size) cases
  where
    edges = extract g size
    graph = mkGraph edges
    cases = [ bench name $ nf (algorithm i) $!! graph | (Named name i) <- inputs edges ]

allBenchs :: (GraphImpl g, NFData g) => [(String,Int)] -> [NSuite g] -> [Benchmark]
allBenchs gr = map (benchmark $ graphs gr)

benchmarkCreation :: (NFData g) => [(String,Int)] -> (Edges -> g) -> [Benchmark]
benchmarkCreation gr mk = [ bgroup ("make a " ++  n ++ " from a list") $ map (\i -> bench (show i) $ nf mk $ grf i ) ss | (Named n grf, ss) <- graphs gr ]

---- Weigh
weigh :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> NSuite g -> Weigh ()
weigh graphs (Named sname (Suite algo inputs)) = wgroup sname cases
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

allWeighs :: (GraphImpl g, NFData g) => [NSuite g] -> Weigh ()
allWeighs = mapM_ (weigh $ graphs defaultGr)

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
weighCreationList = [ Named (str n) t | t@(Named n _, _) <- graphs defaultGr]
  where
    str n = "make a " ++ n ++ " from a list"

---- DataSize

computeSize :: (NFData g) => [(String,Int)] -> (Edges -> g) -> IO [Named [Named Word]]
computeSize gr fun = mapM (\(g,ss) -> sequence $ Named (show g) $ mapM (\s -> sequence $ Named (show s) $ recursiveSize $!! fun $ extract g s) ss) $ graphs gr

