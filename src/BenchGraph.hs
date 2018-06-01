{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}

module BenchGraph (
  ShadowedS (..),
  Suite (..),
  simpleSuite,
  GraphImpl,
  mkGraph,
  benchmark,
  weigh,
  allBench,
  allWeigh,
  benchmarkCreation,
  weighCreation,
  weighCreationList,
  computeSize,
  extractDescription
) where

import Criterion.Main
import Criterion.Types (Benchmark (..))
import Weigh
import GHC.DataSize

import Control.DeepSeq (NFData, ($!!))
import Control.Monad (when)

import BenchGraph.GenericGraph
import BenchGraph.Utils (graphs, defaultGr)
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
simpleSuite name desc algorithm = Suite name desc (const algorithm) (const [("",())])

-- An interface between our generic graphs and others
class GraphImpl g where
    mkGraph :: Edges -> g

---- Criterion
-- | Main function, will benchmark the given suite against the given graphs
benchmark :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> Suite g -> Benchmark
benchmark graphs (Suite sname _ algo inputs) = bgroup sname cases
  where
    cases = [ bgroup gname $ map (benchSuite algo inputs gfunc) ss | ((gname,gfunc), ss) <- graphs ]

benchSuite :: (GraphImpl g, NFData g, NFData o)
           => (i -> g -> o) -> (Edges -> [Named i]) -> (Size -> Edges) -> Size -> Benchmark
benchSuite algorithm inputs gfunc size = bgroup (show size) cases
  where
    edges = gfunc size
    graph = mkGraph edges
    cases = [ bench name $ nf (algorithm i) $!! graph | (name,i) <- inputs edges ]

allBench :: (GraphImpl g, NFData g) => [(String,Int)] -> Suite g -> Benchmark
allBench gr = benchmark (graphs gr)

benchmarkCreation :: (NFData g) => [(String,Int)] -> (Edges -> g) -> [Benchmark]
benchmarkCreation gr mk = [ bgroup ("make a " ++  n ++ " from a list of edges") $ map (\i -> bench (show i) $ nf mk $ grf i ) ss | ((n,grf), ss) <- graphs gr ]

---- Weigh
-- | Main function, will benchmark the given suite against the given graphs
weigh :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> Suite g -> Weigh ()
weigh graphs (Suite sname _ algo inputs) = wgroup sname cases
  where
    cases = mapM_ (uncurry mkGroup) graphs
    mkGroup (gname, gfunc) ss = wgroup gname $ mapM_ (weighSuite algo inputs gfunc) ss

weighSuite :: (GraphImpl g, NFData g, NFData o)
           => (i -> g -> o) -> (Edges -> [Named i]) -> (Size -> Edges) -> Size -> Weigh ()
weighSuite algorithm inputs gfunc size = wgroup (show size) cases
  where
    edges = gfunc size
    graph = mkGraph edges
    cases = mapM_ (uncurry wFunc) $ inputs edges
    wFunc name i = func name (algorithm i) $!! graph

allWeigh :: (GraphImpl g, NFData g) => Suite g -> Weigh ()
allWeigh = weigh (graphs defaultGr)

-- | Use the list from weighCreationList
weighCreation :: (NFData g)
              => Maybe [String] -- ^ Maybe selected benchs to do
              -> (Edges -> g) -- ^ A graph-creator function, typically from the GraphImpl class
              -> Weigh ()
weighCreation names mk = mapM_ (\(str,((n,grf), ss)) -> wgroup str $ mapM_ (\i -> func (show i) mk $ grf i ) ss ) $ maybe id (\ols -> filter (\x -> fst x `elem` ols)) names weighCreationList

-- | List of generic graph with their case-name
weighCreationList :: [Named (GenericGraph, [Int])]
weighCreationList = [ (str n,t) | t@((n, _), _) <- graphs defaultGr]
  where
    str n = "make a " ++ n ++ " from a list of edges"

---- DataSize
computeSize :: (NFData g) => [(String,Int)] -> (Edges -> g) -> IO [Named [Named Word]]
computeSize gr fun = mapM (\((gname, gfunc),ss) -> sequence $ (gname,) $ mapM (\s -> sequence $ (show s,) $ recursiveSize $!! fun $ gfunc s) ss) $ graphs gr

---- Utils

extractDescription :: Suite a -> Named String
extractDescription (Suite name desc _ _) = (name,desc)
