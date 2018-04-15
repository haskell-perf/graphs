module BenchGraph.Utils
(tenPowers)
where

tenPowers :: [Int]
tenPowers = 1: map (10*) tenPowers
