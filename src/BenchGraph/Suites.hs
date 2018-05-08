module BenchGraph.Suites
  (
  hasEdgeS,
  addVertexS,
  removeVertexS,
  eqS
  )
where

import BenchGraph
import BenchGraph.GenericGraph
import BenchGraph.Utils

import Control.DeepSeq (NFData)
import Control.Arrow (second)

-- | Type to express the common interface between Specialised suites
type SpecialisedSuite u o i g = (i -> g -> o) -- ^ The actual function to test.
  -> (u -> i) -- ^ A function to create the tested function argument
  -> Suite g

hasEdgeS :: NFData o => SpecialisedSuite (Vertex,Vertex) o i g
hasEdgeS fun genArg = Suite { suiteName = "HasEdge (not in graph)"
                            , algorithm = fun
                            , inputs    = map (second genArg) . withNames . take 2 . edgesNotInGraph }

addVertexS :: NFData o => SpecialisedSuite Vertex o i g
addVertexS fun genArg = Suite { suiteName = "Add a new vertex"
                              , algorithm = fun
                              , inputs    = \x -> [("new vertex: " ++ show (getNewV x), genArg $ getNewV x)]}
    where
      getNewV x = 1 + extractMaxVertex x

removeVertexS :: NFData o => SpecialisedSuite Vertex o i g
removeVertexS fun genArg = Suite { suiteName = "Remove a vertex"
                                 , algorithm = fun
                                 , inputs    = \x -> [("vertex: " ++ show (getOldV x), genArg $ getOldV x)]}
    where
      getOldV x = extractMaxVertex x - 1

eqS :: (NFData g, GraphImpl g) => (g -> g -> Bool) -> Suite g
eqS fun = Suite { suiteName = "Equality"
                , algorithm = fun
                , inputs    = \x -> map (second mkGraph)
                  [("One edge: "++ show (head x), [head x])
                  ,("Same graph", x)
                  ]
                }
