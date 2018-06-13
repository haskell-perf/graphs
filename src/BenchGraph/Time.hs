module BenchGraph.Time (
  benchmark,
  allBench,
  benchmarkCreation
) where

import Criterion.Main
import Criterion.Types (Benchmark (..))

import Control.DeepSeq (NFData, ($!!))

import BenchGraph.GenericGraph
import BenchGraph.Utils (graphs)
import BenchGraph.Named
import BenchGraph.Types

---- Criterion
-- | Main function, will benchmark the given suite against the given graphs
benchmark :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> Suite g -> Benchmark
benchmark graphs' (Suite sname _ algo inputs') = bgroup sname cases
  where
    cases = [ bgroup gname $ map (benchSuite algo inputs' gfunc) ss | ((gname,gfunc), ss) <- graphs' ]

benchSuite :: (GraphImpl g, NFData g, NFData o)
           => (i -> g -> o) -> (Edges -> [Named i]) -> (Size -> (Edges,Int)) -> Size -> Benchmark
benchSuite algorithm' inputs' gfunc size = bgroup (show sizeName) cases
  where
    (edges, sizeName) = gfunc size
    graph = case edges of
              [] -> mkVertex
              edgs -> mkGraph edgs
    cases = [ bench name' $ nf (algorithm' i) $!! graph | (name',i) <- inputs' edges ]

allBench :: (GraphImpl g, NFData g) => [(String,Int)] -> Suite g -> Benchmark
allBench gr = benchmark (graphs gr)

benchmarkCreation :: (NFData g) => [(String,Int)] -> (Edges -> g) -> Benchmark
benchmarkCreation gr mk = bgroup "creation" [ bgroup n $ map (\i -> bgroup (show i) [bench "" $ nf mk $ fst $ grf i] ) ss | ((n,grf), ss) <- graphs gr ]

