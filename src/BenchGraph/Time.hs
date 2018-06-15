module BenchGraph.Time (
  benchmark,
  allBench,
  benchmarkCreation,
  benchmarkWithCreation,
  benchmarkWithoutCreation
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

benchmarkWithCreation :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> Suite g -> Benchmark
benchmarkWithCreation = benchmark True

benchmarkWithoutCreation :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> Suite g -> Benchmark
benchmarkWithoutCreation = benchmark False

benchmark :: (GraphImpl g, NFData g) 
          => Bool -- ^ Set to False, it will force the graph, using deepseq, before passing it to the benched function
          -> [(GenericGraph, [Size])] -> Suite g -> Benchmark
benchmark benchCreation graphs' (Suite sname _ algo inputs') = bgroup sname cases
  where
    cases = [ bgroup gname $ map (benchSuite benchCreation algo inputs' gfunc) ss | ((gname,gfunc), ss) <- graphs' ]

benchSuite :: (GraphImpl g, NFData g, NFData o)
           => Bool -> (i -> g -> o) -> (Edges -> [Named i]) -> (Size -> (Edges,Int)) -> Size -> Benchmark
benchSuite benchCreation algorithm' inputs' gfunc size = bgroup (show sizeName) cases
  where
    (edges, sizeName) = gfunc size
    !graph = case edges of
              [] -> const mkVertex
              _ -> mkGraph
    cases = if benchCreation
               then [ bench name' $ nf (algorithm' i . graph) $!! edges | (name',i) <- inputs' edges ]
               else [ bench name' $ nf (algorithm' i) $!! graph edges | (name',i) <- inputs' edges ]

allBench :: (GraphImpl g, NFData g)
         => Bool -- ^ Do we bench creation of the graph ?
         -> [(String,Int)] -> Suite g -> Benchmark
allBench benchCreation gr = benchmark benchCreation (graphs gr)

benchmarkCreation :: (NFData g) => [(String,Int)] -> (Edges -> g) -> Benchmark
benchmarkCreation gr mk = bgroup "creation" [ bgroup n $ map (\i -> let (gr',sizeName) = grf i in bgroup (show sizeName) [bench "" $ nf mk gr'] ) ss | ((n,grf), ss) <- graphs gr ]

