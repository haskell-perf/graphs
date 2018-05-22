module BenchGraph.Suites

where

import BenchGraph
import BenchGraph.GenericGraph
import BenchGraph.Utils
import BenchGraph.Named

import Control.DeepSeq (NFData)

-- | Type to express the common interface between Specialised suites
type SpecialisedSuite u o i g = (i -> g -> o) -- ^ The actual function to test.
  -> (u -> i) -- ^ A function to create the tested function argument
  -> Suite g

-- Vertex work

vertexList :: NFData o => (g -> o) -> Suite g
vertexList = simpleSuite "vertexList"

vertexCount :: NFData o => (g -> o) -> Suite g
vertexCount = simpleSuite "vertexCount"

addVertex :: NFData o => SpecialisedSuite Vertex o i g
addVertex fun genArg = Suite
  { suiteName = "add a new vertex"
  , algorithm = fun
  , inputs    = \x -> fmap genArg <$> [nameBy ((++)"new vertex: " . show ) $ getNewV x]
  }
    where
      getNewV x = 1 + extractMaxVertex x

removeVertex :: NFData o => SpecialisedSuite Vertex o i g
removeVertex fun genArg = Suite
  { suiteName = "remove a vertex"
  , algorithm = fun
  , inputs    = \x -> fmap genArg <$> [nameBy ((++)"vertex: " . show ) $ getOldV x]
  }
    where
      getOldV x = extractMaxVertex x - 1

-- Edge work

edgeList :: NFData o => (g -> o) -> Suite g
edgeList = simpleSuite "edgeList"

edgeCount :: NFData o => (g -> o) -> Suite g
edgeCount = simpleSuite "edgeCount"

hasEdge :: NFData o => SpecialisedSuite Edge o i g
hasEdge fun genArg = Suite
  { suiteName = "hasEdge"
  , algorithm = fun
  , inputs    = map (fmap genArg) . withNames . take 2 . edgesNotInGraph
  }

addEdge :: NFData o => SpecialisedSuite Edge o i g
addEdge fun genArg = Suite
  { suiteName = "add a new edge"
  , algorithm = fun
  , inputs = map (fmap genArg) . withNames . take 2 . edgesNotInGraph
  }

removeEdge :: NFData o => SpecialisedSuite Edge o i g
removeEdge fun genArg = Suite
  { suiteName = "remove an edge"
  , algorithm = fun
  , inputs    = map (fmap genArg) . withNames . take 2
  }

-- Graph work

isEmpty :: NFData o => (g -> o) -> Suite g
isEmpty = simpleSuite "isEmpty"

transpose :: NFData o => (g -> o) -> Suite g
transpose = simpleSuite "transpose"

eq :: (NFData g, GraphImpl g) => (g -> g -> Bool) -> Suite g
eq fun = Suite
  { suiteName = "equality"
  , algorithm = fun
  , inputs    = \x -> fmap mkGraph <$>
    [nameBy ((++)"vertex: " . show . head) [(0,2)]
    ,Named "Same graph" x
    ]
  }

context :: NFData o => SpecialisedSuite Edge o i g
context fun genArg = Suite
  { suiteName = "merge a context"
  , algorithm = fun
  , inputs = map (fmap genArg) . withNames . const [(0,3)]
  }
