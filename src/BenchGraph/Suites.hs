{-# LANGUAGE TupleSections #-}

module BenchGraph.Suites

where

import BenchGraph
import BenchGraph.GenericGraph
import BenchGraph.Utils
import BenchGraph.Named

import Control.DeepSeq (NFData)
import Data.List (nub)

-- | Type to express the common interface between Specialised suites
type SpecialisedSuite u o i g = (i -> g -> o) -- ^ The actual function to test.
  -> (u -> i) -- ^ A function to create the tested function argument
  -> NSuite g

-- Vertex work

vertexList :: NFData o => (g -> o) -> NSuite g
vertexList = simpleSuite "vertexList"

vertexCount :: NFData o => (g -> o) -> NSuite g
vertexCount = simpleSuite "vertexCount"

addVertex :: NFData o => SpecialisedSuite Vertex o i g
addVertex fun genArg = ("add a new vertex",) $ Suite
  { algorithm = fun
  , inputs    = \x -> fmap genArg <$> [nameBy ((++)"new vertex: " . show ) $ getNewV x]
  }
    where
      getNewV x = 1 + extractMaxVertex x

removeVertex :: NFData o => SpecialisedSuite Vertex o i g
removeVertex fun genArg = ("remove a vertex",) $ Suite
  { algorithm = fun
  , inputs    = \x -> fmap genArg <$> [nameBy ((++)"vertex: " . show ) $ getOldV x]
  }
    where
      getOldV x = extractMaxVertex x - 1

-- Edge work

edgeList :: NFData o => (g -> o) -> NSuite g
edgeList = simpleSuite "edgeList"

edgeCount :: NFData o => (g -> o) -> NSuite g
edgeCount = simpleSuite "edgeCount"

hasEdge :: NFData o => SpecialisedSuite Edge o i g
hasEdge fun genArg = ("hasEdge",) $ Suite
  { algorithm = fun
  , inputs    = map (fmap genArg) . withNames . getDifferents . edgesNotInGraph
  }

addEdge :: NFData o => SpecialisedSuite Edge o i g
addEdge fun genArg = ("add a new edge",) $ Suite
  { algorithm = fun
  , inputs = map (fmap genArg) . withNames . getDifferents . edgesNotInGraph
  }

removeEdge :: NFData o => SpecialisedSuite Edge o i g
removeEdge fun genArg = ("remove an edge",) $ Suite
  { algorithm = fun
  , inputs    = map (fmap genArg) . withNames . getDifferents
  }

-- Graph work

isEmpty :: NFData o => (g -> o) -> NSuite g
isEmpty = simpleSuite "isEmpty"

transpose :: NFData o => (g -> o) -> NSuite g
transpose = simpleSuite "transpose"

eq :: (NFData g, GraphImpl g) => (g -> g -> Bool) -> NSuite g
eq fun = ("equality",) $ Suite
  { algorithm = fun
  , inputs    = \x -> fmap mkGraph <$>
    [nameBy ((++)"vertex: " . show . head) [(0,2)]
    ,("Same graph",x)
    ]
  }

context :: NFData o => SpecialisedSuite Edge o i g
context fun genArg = ("merge a context",) $ Suite
  { algorithm = fun
  , inputs = map (fmap genArg) . withNames . const [(0,3)]
  }

-- Algorithms

dff :: NFData o => (g -> o) -> NSuite g
dff = simpleSuite "dff"

topSort :: NFData o => (g -> o) -> NSuite g
topSort = simpleSuite "topSort"

reachable :: NFData o => SpecialisedSuite Vertex o i g
reachable fun genArg = ("reachable",) $ Suite
  { algorithm = fun
  , inputs    = map (fmap genArg) . withNames . const [0]
  }

-- Utils

-- | Take the first, the middle and the last edges, if possible
getDifferents :: Edges -> Edges
getDifferents edgs = if length edgs >= 2
                        then nub [head edgs, edgs !! (length edgs `div` 2 - 1), last $ init edgs]
                        else edgs
