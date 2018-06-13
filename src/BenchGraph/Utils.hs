module BenchGraph.Utils
  (
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
import BenchGraph.Named
import BenchGraph.RealLife.Graphs

import Weigh (Weigh, Grouped, Weight, weighResults)
import System.Environment (lookupEnv)
import Control.Monad (unless)
import Data.Maybe (isJust, mapMaybe)

-- | Remove given edges from the complete graph
edgesNotInGraph :: Edges -> Edges
edgesNotInGraph edgs = (\\) (fst (snd complete $ extractMaxVertex edgs + 1)) edgs

extractMaxVertex :: Edges -> Int
extractMaxVertex = foldl (\act (v1,v2) -> max act (max v1 v2)) 0

graphs :: [(String, Int)] -> [(GenericGraph, [Int])]
graphs = mapMaybe (\(x,y) -> (\n -> (defaultGraphs !! n,[0..(y-1)])) <$> elemIndex x graphsNames)

defaultGraphs :: [GenericGraph]
defaultGraphs = [path, circuit, mesh, complete, clique, realLife]

defaultGr :: [Named Int]
defaultGr = [("Mesh",3),("Clique",3)]

graphsNames :: [String]
graphsNames = map fst defaultGraphs

vertices :: Edges -> [Vertex]
vertices = nub . uncurry (++) . unzip

mainWeigh :: Weigh () -> ([Grouped (Weight, Maybe String)] -> IO ()) -> IO ()
mainWeigh wei f = do
  args <- lookupEnv "WEIGH_CASE"
  (results,_) <- weighResults wei
  unless (isJust args) $ f results
