module BenchGraph.Suites
  (
  hasEdgeS,
  addVertexS,
  removeVertexS
  )
where

import BenchGraph
import BenchGraph.GenericGraph
import BenchGraph.Utils

import Control.DeepSeq (NFData)

hasEdgeS :: NFData o => (i -> g -> o) -> ((String,(Vertex,Vertex)) -> (String,i))-> Suite g
hasEdgeS fun genArg = Suite { suiteName = "hasEdge (not in graph)"
                             , algorithm = fun
                       , inputs    = map genArg . withNames . take 2 . edgesNotInGraph }

addVertexS :: NFData o => (i -> g -> o) -> (Int -> i) -> Suite g
addVertexS fun genArg = Suite { suiteName = "add a new vertex"
                              , algorithm = fun
                              , inputs    = \x -> [("new vertex: " ++ show (getNewV x), genArg $ getNewV x)]}
    where
      getNewV x = 1 + extractMaxVertex x

removeVertexS :: NFData o => (i -> g -> o) -> (Int -> i) -> Suite g
removeVertexS fun genArg = Suite { suiteName = "remove a vertex"
                                 , algorithm = fun
                                 , inputs    = \x -> [("vertex: " ++ show (getOldV x), genArg $ getOldV x)]}
    where
      getOldV x = extractMaxVertex x - 1
