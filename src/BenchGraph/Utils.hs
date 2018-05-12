module BenchGraph.Utils (
tenPowers,
edgesNotInGraph,
extractMaxVertex,
graphs,
mainWeigh
)
where

import Data.List ((\\), isInfixOf)
import BenchGraph.GenericGraph
import BenchGraph.Complete
import BenchGraph.Circuit
import BenchGraph.Path

import Weigh (mainWith, Weigh)
import System.Environment (getArgs)
import Control.Monad (unless)

tenPowers :: [Int]
tenPowers = iterate (10*) 1

-- | Remove given edges from the complete graph
edgesNotInGraph :: Edges -> Edges
edgesNotInGraph edgs = (\\) (mk complete  $ extractMaxVertex edgs) edgs

extractMaxVertex :: Edges -> Int
extractMaxVertex = foldl (\act (v1,v2) -> max act (max v1 v2)) 0

graphs :: [(GenericGraph, [Int])]
graphs = take 1 [
  (path, take 1 tenPowers),
  (circuit, take 5 tenPowers),
  (complete, take 3 tenPowers)
  ]

mainWeigh :: Weigh () -> IO () -> IO ()
mainWeigh wei rest = do
  args <- getArgs
  mainWith wei
  unless (foldl (\x y -> x || isInfixOf "--case" y) False args) rest
