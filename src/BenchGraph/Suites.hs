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

vertexListS :: NFData o => (g -> o) -> Suite g
vertexListS = simpleSuite "vertexList"

addVertexS :: NFData o => SpecialisedSuite Vertex o i g
addVertexS fun genArg = Suite
  { suiteName = "add a new vertex"
  , algorithm = fun
  , inputs    = \x -> fmap genArg <$> [nameBy ((++)"new vertex: " . show ) $ getNewV x]
  }
    where
      getNewV x = 1 + extractMaxVertex x

removeVertexS :: NFData o => SpecialisedSuite Vertex o i g
removeVertexS fun genArg = Suite
  { suiteName = "remove a vertex"
  , algorithm = fun
  , inputs    = \x -> fmap genArg <$> [nameBy ((++)"vertex: " . show ) $ getOldV x]
  }
    where
      getOldV x = extractMaxVertex x - 1

-- Edge work

edgeListS :: NFData o => (g -> o) -> Suite g
edgeListS = simpleSuite "edgeList"

hasEdgeS :: NFData o => SpecialisedSuite Edge o i g
hasEdgeS fun genArg = Suite
  { suiteName = "hasEdge"
  , algorithm = fun
  , inputs    = map (fmap genArg) . withNames . take 2 . edgesNotInGraph
  }

addEdgeS :: NFData o => SpecialisedSuite Edge o i g
addEdgeS fun genArg = Suite
  { suiteName = "add a new edge"
  , algorithm = fun
  , inputs = map (fmap genArg) . withNames . take 2 . edgesNotInGraph
  }

removeEdgeS :: NFData o => SpecialisedSuite Edge o i g
removeEdgeS fun genArg = Suite
  { suiteName = "remove an edge"
  , algorithm = fun
  , inputs    = map (fmap genArg) . withNames . take 2
  }

-- Graph work

isEmptyS :: NFData o => (g -> o) -> Suite g
isEmptyS = simpleSuite "isEmpty"

transposeS :: NFData o => (g -> o) -> Suite g
transposeS = simpleSuite "transpose"

eqS :: (NFData g, GraphImpl g) => (g -> g -> Bool) -> Suite g
eqS fun = Suite
  { suiteName = "equality"
  , algorithm = fun
  , inputs    = \x -> fmap mkGraph <$>
    [nameBy ((++)"vertex: " . show . head) [(0,2)]
    ,Named "Same graph" x
    ]
  }

contextS :: NFData o => SpecialisedSuite Edge o i g
contextS fun genArg = Suite
  { suiteName = "merge a context"
  , algorithm = fun
  , inputs = map (fmap genArg) . withNames . const [(0,3)]

  }
