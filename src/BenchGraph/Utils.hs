module BenchGraph.Utils
  (
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
import BenchGraph.Named

import Control.Comonad (extract)

import Weigh (mainWith, Weigh, Grouped, Weight, weighResults)
import System.Environment (getArgs)
import Control.Monad (unless)

tenPowers :: [Int]
tenPowers = iterate (10*) 1

-- | Remove given edges from the complete graph
edgesNotInGraph :: Edges -> Edges
edgesNotInGraph edgs = (\\) (extract complete  $ extractMaxVertex edgs) edgs

extractMaxVertex :: Edges -> Int
extractMaxVertex = foldl (\act (v1,v2) -> max act (max v1 v2)) 0

graphs :: (Int, Int, Int) -> [(GenericGraph, [Int])]
graphs (a,b,c) = [
  (path, take a tenPowers),
  (circuit, take b tenPowers),
  (complete, take c tenPowers)
  ]

mainWeigh :: Weigh () -> ([Grouped (Weight, Maybe String)] -> IO ()) -> IO ()
mainWeigh wei f = do
  args <- getArgs
  (results,_) <- weighResults wei
  unless (foldl (\x y -> x || isInfixOf "--case" y) False args) $ f results
