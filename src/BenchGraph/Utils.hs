module BenchGraph.Utils
  (
  tenPowers,
  edgesNotInGraph,
  extractMaxVertex,
  graphs,
  mainWeigh,
  vertices,
  defaultGr,
  graphsNames
  )

where

import Data.List ((\\), nub, elemIndex)
import BenchGraph.GenericGraph
import BenchGraph.GenericGraph.Complete
import BenchGraph.GenericGraph.Circuit
import BenchGraph.GenericGraph.Path
import BenchGraph.GenericGraph.Mesh
import BenchGraph.GenericGraph.Clique
import BenchGraph.Named

import Weigh (mainWith, Weigh, Grouped, Weight, weighResults)
import System.Environment (lookupEnv)
import Control.Monad (unless)
import Data.Maybe (isJust, mapMaybe)

tenPowers :: [Int]
tenPowers = iterate (10*) 1

-- | Remove given edges from the complete graph
edgesNotInGraph :: Edges -> Edges
edgesNotInGraph edgs = (\\) (snd complete $ extractMaxVertex edgs + 1) edgs

extractMaxVertex :: Edges -> Int
extractMaxVertex = foldl (\act (v1,v2) -> max act (max v1 v2)) 0

graphs :: [(String, Int)] -> [(GenericGraph, [Int])]
graphs = mapMaybe (\(x,y) -> (\n -> (defaultGraphs !! n, take y tenPowers)) <$> elemIndex x graphsNames)

defaultGraphs :: [GenericGraph]
defaultGraphs = [path, circuit, mesh, complete, clique]

defaultGr :: [Named Int]
defaultGr = [("Mesh",3),("Clique",2)]

graphsNames :: [String]
graphsNames = map fst defaultGraphs

defaultSizeGraph ::[Int]
defaultSizeGraph = [3,3,3,2]

vertices :: Edges -> [Vertex]
vertices = nub . uncurry (++) . unzip

mainWeigh :: Weigh () -> ([Grouped (Weight, Maybe String)] -> IO ()) -> IO ()
mainWeigh wei f = do
  args <- lookupEnv "WEIGH_CASE"
  (results,_) <- weighResults wei
  unless (isJust args) $ f results
