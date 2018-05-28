module BenchGraph.Utils
  (
  tenPowers,
  edgesNotInGraph,
  extractMaxVertex,
  graphs,
  mainWeigh,
  vertices,
  defaultSizeGraph,
  defaultGraphsNames
  )

where

import Data.List ((\\), nub)
import BenchGraph.GenericGraph
import BenchGraph.Complete
import BenchGraph.Circuit
import BenchGraph.Path
import BenchGraph.Mesh
import BenchGraph.Named

import Control.Comonad (extract)

import Weigh (mainWith, Weigh, Grouped, Weight, weighResults)
import System.Environment (lookupEnv)
import Control.Monad (unless)
import Data.Maybe (isJust)

tenPowers :: [Int]
tenPowers = iterate (10*) 1

-- | Remove given edges from the complete graph
edgesNotInGraph :: Edges -> Edges
edgesNotInGraph edgs = (\\) (extract complete  $ extractMaxVertex edgs) edgs

extractMaxVertex :: Edges -> Int
extractMaxVertex = foldl (\act (v1,v2) -> max act (max v1 v2)) 0

graphs :: [String] -> [Int] -> [(GenericGraph, [Int])]
graphs grNames = zip (filter (\x -> show x `elem` grNames) defaultGraphs) . map (`take` tenPowers)

defaultGraphs :: [GenericGraph]
defaultGraphs = [path, circuit, mesh, complete]

defaultGraphsNames :: [String]
defaultGraphsNames = map show defaultGraphs

defaultSizeGraph ::[Int]
defaultSizeGraph = [3,3,3,2]

vertices :: Edges -> [Vertex]
vertices = nub . uncurry (++) . unzip

mainWeigh :: Weigh () -> ([Grouped (Weight, Maybe String)] -> IO ()) -> IO ()
mainWeigh wei f = do
  args <- lookupEnv "WEIGH_CASE"
  (results,_) <- weighResults wei
  unless (isJust args) $ f results
