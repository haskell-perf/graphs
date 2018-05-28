module BenchGraph.Utils
  (
  tenPowers,
  edgesNotInGraph,
  extractMaxVertex,
  graphs,
  mainWeigh,
  vertices,
  SizeGraph,
  defaultSizeGraph
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

type SizeGraph = (Int,Int,Int,Int)

tenPowers :: [Int]
tenPowers = iterate (10*) 1

-- | Remove given edges from the complete graph
edgesNotInGraph :: Edges -> Edges
edgesNotInGraph edgs = (\\) (extract complete  $ extractMaxVertex edgs) edgs

extractMaxVertex :: Edges -> Int
extractMaxVertex = foldl (\act (v1,v2) -> max act (max v1 v2)) 0

graphs :: SizeGraph -> [(GenericGraph, [Int])]
graphs (a,b,c,d) = [
  (path, take a tenPowers),
  (circuit, take b tenPowers),
  (mesh, take c tenPowers),
  (complete, take d tenPowers)
  ]

defaultSizeGraph :: SizeGraph
defaultSizeGraph = (3,3,3,2)

vertices :: Edges -> [Vertex]
vertices = nub . uncurry (++) . unzip

mainWeigh :: Weigh () -> ([Grouped (Weight, Maybe String)] -> IO ()) -> IO ()
mainWeigh wei f = do
  args <- lookupEnv "WEIGH_CASE"
  (results,_) <- weighResults wei
  unless (isJust args) $ f results
