{-|
This module describe main suites to be used
-}

module BenchGraph.Suites

where

import BenchGraph.Types
import BenchGraph.GenericGraph
import BenchGraph.Utils
import BenchGraph.Named

import Control.DeepSeq (NFData)
import Data.List (nub)

-- | Type to express the common interface between Specialised suites
type SpecialisedSuite u o i g = (i -> g -> o) -- ^ The actual function to test.
  -> (u -> i) -- ^ A function to create the tested function argument
  -> Suite g

-- Vertex work

vertexList :: NFData o => (g -> o) -> Suite g
vertexList = simpleSuite "vertexList" "Produce a list of the vertices in the graph"

vertexCount :: NFData o => (g -> o) -> Suite g
vertexCount = simpleSuite "vertexCount" "Count the vertices of the graph"

hasVertex :: NFData o => SpecialisedSuite Vertex o i g
hasVertex fun genArg = Suite
  { name = "hasVertex"
  , desc = "Test if the given vertex is in the graph"
  , algorithm = fun
  , inputs    = map (fmap genArg) . withNames . vertices
  }
    where
      vertices x = nub $ 0 : maxV x `div` 2 : [maxV x + 1]
      maxV = extractMaxVertex

addVertex :: NFData o => SpecialisedSuite Vertex o i g
addVertex fun genArg = Suite
  { name = "addVertex"
  , desc = "Add a vertex (not already in the graph)"
  , algorithm = fun
  , inputs    = \x -> fmap genArg <$> [nameBy ((++)"new vertex: " . show ) $ getNewV x]
  }
    where
      getNewV x = 1 + extractMaxVertex x

removeVertex :: NFData o => SpecialisedSuite Vertex o i g
removeVertex fun genArg = Suite
  { name = "removeVertex"
  , desc = "Remove a vertex of the graph"
  , algorithm = fun
  , inputs    = \x -> fmap genArg <$> [nameBy ((++)"vertex: " . show ) $ getOldV x]
  }
    where
      getOldV x = extractMaxVertex x - 1

-- Edge work

edgeList :: NFData o => (g -> o) -> Suite g
edgeList = simpleSuite "edgeList" "Produce a list of the edges in the graph"

edgeCount :: NFData o => (g -> o) -> Suite g
edgeCount = simpleSuite "edgeCount" "Count the edges of the graph"

hasEdge :: NFData o => SpecialisedSuite Edge o i g
hasEdge fun genArg = Suite
  { name = "hasEdge"
  , desc = "Test if the given edge is in the graph (with arguments both in the graph and not in the graph (where applicable))"
  , algorithm = fun
  , inputs    = \x -> map (fmap genArg) $ withNames $ take 2 x ++ take 2 (edgesNotInGraph x)
  }

addEdge :: NFData o => SpecialisedSuite Edge o i g
addEdge fun genArg = Suite
  { name = "addEdge"
  , desc = "Add an edge (not already in the graph)"
  , algorithm = fun
  , inputs = map (fmap genArg) . withNames . take 4 . edgesNotInGraph
  }

removeEdge :: NFData o => SpecialisedSuite Edge o i g
removeEdge fun genArg = Suite
  { name = "removeEdge"
  , desc = "Remove an edge of the graph"
  , algorithm = fun
  , inputs    = map (fmap genArg) . withNames . getDifferents
  }

-- Graph work

isEmpty :: NFData o => (g -> o) -> Suite g
isEmpty = simpleSuite "isEmpty" "Test if the graph is empty"

transpose :: NFData o => (g -> o) -> Suite g
transpose = simpleSuite "transpose" "Transpose (invert all the edges) the graph"

eq :: (NFData g, GraphImpl g) => (g -> g -> Bool) -> Suite g
eq fun = Suite
  { name = "equality"
  , desc = "Test if two graphs are equals"
  , algorithm = fun
  , inputs    = \x -> fmap mkGraph <$>
    [nameBy ((++)"vertex: " . show . head) [(0,2)]
    ,("Same graph",x)
    ]
  }

context :: NFData o => SpecialisedSuite Edge o i g
context fun genArg = Suite
  { name = "mergeContext"
  , desc = "Merge a FGL context in the graph"
  , algorithm = fun
  , inputs = map (fmap genArg) . withNames . const [(0,3)]
  }

-- Algorithms

dff :: NFData o => (g -> o) -> Suite g
dff = simpleSuite "dff" "Produce a forest, obtainened from a DFS (Deep First Search) of each vertex"

topSort :: NFData o => (g -> o) -> Suite g
topSort = simpleSuite "topSort" "Topological sorting of the vertices"

reachable :: NFData o => SpecialisedSuite Vertex o i g
reachable fun genArg = Suite
  { name = "reachable"
  , desc = "Produce a list of reachable vertices from a given one"
  , algorithm = fun
  , inputs    = map (fmap genArg) . withNames . const [0]
  }

-- Utils

-- | Take the first, the middle and the last edges, if possible
getDifferents :: Edges -> Edges
getDifferents edgs = if length edgs >= 2
                        then nub [head edgs, edgs !! (length edgs `div` 2 - 1), last $ init edgs]
                        else edgs
