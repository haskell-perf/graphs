module BenchGraph.Utils
  (
  edgesNotInGraph,
  extractMaxVertex,
  graphs,
  vertices,
  defaultGr,
  graphsNames
  )

where

import Data.List ((\\), nub, elemIndex)
import BenchGraph.GenericGraph
import BenchGraph.Named
import BenchGraph.RealLife.Graphs

import Data.Maybe (mapMaybe)

-- | Remove given edges from the complete graph
edgesNotInGraph :: Edges -> Edges
edgesNotInGraph edgs = (\\) (mkCompleteDir $ extractMaxVertex edgs + 1) edgs

extractMaxVertex :: Edges -> Int
extractMaxVertex = foldl (\act (v1,v2) -> max act (max v1 v2)) 0

graphs :: Bool -> [(String, Int)] -> [(GenericGraph, [Int])]
graphs b = mapMaybe (\(x,y) -> (\n -> (defaultGraphs !! n,[(if b then y-1 else 0)..(y-1)])) <$> elemIndex x graphsNames)

defaultGraphs :: [GenericGraph]
defaultGraphs = 
  [ path
  , circuit
  , mesh
  , complete
  , clique
  , realLife
  ]

defaultGr :: [Named Int]
defaultGr = [("Mesh",3),("Clique",3)]

graphsNames :: [String]
graphsNames = map fst defaultGraphs

vertices :: Edges -> [Vertex]
vertices = nub . uncurry (++) . unzip

