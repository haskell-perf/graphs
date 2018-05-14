module BenchGraph.Suites
  (
  hasEdgeS,
  addVertexS,
  removeVertexS,
  eqS,
  isEmptyS,
  edgeListS,
  vertexListS
  )
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

hasEdgeS :: NFData o => SpecialisedSuite Edge o i g
hasEdgeS fun genArg = Suite { suiteName = "hasEdge (not in graph)"
                            , algorithm = fun
                            , inputs    = map (fmap genArg) . withNames . take 2 . edgesNotInGraph }

addVertexS :: NFData o => SpecialisedSuite Vertex o i g
addVertexS fun genArg = Suite { suiteName = "add a new vertex"
                              , algorithm = fun
                              , inputs    = \x -> fmap genArg <$> [nameBy ((++)"new vertex: " . show ) $ getNewV x]}
    where
      getNewV x = 1 + extractMaxVertex x

removeVertexS :: NFData o => SpecialisedSuite Vertex o i g
removeVertexS fun genArg = Suite { suiteName = "remove a vertex"
                                 , algorithm = fun
                                 , inputs    = \x -> fmap genArg <$> [nameBy ((++)"vertex: " . show ) $ getOldV x]}
    where
      getOldV x = extractMaxVertex x - 1

eqS :: (NFData g, GraphImpl g) => (g -> g -> Bool) -> Suite g
eqS fun = Suite { suiteName = "equality"
                , algorithm = fun
                , inputs    = \x -> fmap mkGraph <$>
                  [nameBy ((++)"vertex: " . show . head) [(0,2)]
                  ,Named "Same graph" x
                  ]
                }

isEmptyS :: NFData o => (g -> o) -> Suite g
isEmptyS = simpleSuite "IsEmpty"

vertexListS :: NFData o => (g -> o) -> Suite g
vertexListS = simpleSuite "vertexList"

edgeListS :: NFData o => (g -> o) -> Suite g
edgeListS = simpleSuite "edgeList"
