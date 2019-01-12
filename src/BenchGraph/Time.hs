module BenchGraph.Time (
  benchmark,
  allBench,
  benchmarkCreation
) where

import Criterion.Main
import Criterion.Types (Benchmark (..))

import Control.DeepSeq (NFData, deepseq)

import BenchGraph.GenericGraph
import BenchGraph.Utils (graphs)
import BenchGraph.Named
import BenchGraph.Suites (Suite (..))
import BenchGraph.Types

---- Criterion
-- | Main function, will benchmark the given suite against the given graphs

benchmark :: (GraphImpl g, NFData g)
          => Bool -- ^ Set to False, it will force the graph, using deepseq, before passing it to the benched function
          -> [(GenericGraph, [Size])] -> Suite g -> Benchmark
benchmark benchCreation graphs' (Suite sname algo inputs') = bgroup (show sname) cases
  where
    cases = [ bgroup gname $ map (benchSuite benchCreation algo inputs' gfunc) ss | ((gname,gfunc), ss) <- graphs' ]

benchSuite :: (GraphImpl g, NFData g, NFData o)
           => Bool -> (i -> g -> o) -> (Edges -> [Named i]) -> (Size -> (Edges,Int)) -> Size -> Benchmark
benchSuite benchCreation algorithm' inputs' gfunc size = bgroup (show sizeName) cases
  where
    (edges, sizeName) = gfunc size
    graph = case edges of
              [] -> const mkVertex
              _ -> mkGraph
    ge = graph edges
    cases = if benchCreation
               then edges `deepseq` [ bench name' $ nf (algorithm' i . graph) $ edges | (name',i) <- inputs' edges ]
               else ge `deepseq` [ bench name' $ nf (algorithm' i) $ ge | (name',i) <- inputs' edges ]

allBench :: (GraphImpl g, NFData g)
         => Bool -- ^ Do we bench creation of the graph ?
         -> Bool -- ^ Do we use only bigger graphs ?
         -> [(String,Int)] -> Suite g -> Benchmark
allBench benchCreation b gr = benchmark benchCreation (graphs b gr)

benchmarkCreation :: (NFData g) => Bool -> [(String,Int)] -> (Edges -> g) -> Benchmark
benchmarkCreation b gr mk = bgroup "Creation" [ bgroup n $ map (\i -> let (gr',sizeName) = grf i in bgroup (show sizeName) [bench "" $ nf mk gr'] ) ss | ((n,grf), ss) <- graphs b gr ]
