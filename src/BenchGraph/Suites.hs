{-|
This module describe main suites to be used
-}

module BenchGraph.Suites

where

import BenchGraph.Types
import BenchGraph.GenericGraph
import BenchGraph.Utils hiding (vertices)
import BenchGraph.Named

import Control.DeepSeq (NFData)
import Data.List (nub)

-- | Type to express the common interface between Specialised suites
type SpecialisedSuite u i = (u -> i) -- ^ A function to create the tested function argument
  -> (String, String,Edges -> [Named i])

-- Simplifiers

s :: NFData o => (String,String) -> (g -> o) -> Suite g
s (n,d) = simpleSuite n d

sIO :: NFData o => (String,String) -> (g -> IO o) -> SuiteIO g
sIO (n,d) = simpleSuiteIO n d

a :: NFData o => SpecialisedSuite u i -> (i -> g -> o) -> (u -> i) -> Suite g
a ss f genArg = Suite n d f args
  where (n,d,args) = ss genArg

aIO :: NFData o => (String, String,Edges -> [Named i]) -> (i -> g -> IO o) -> SuiteIO g
aIO (n,d,args) f = SuiteIO n d f args

-- Vertex work

vertexList :: (String, String)
vertexList = ("vertexList","Produce a list of the vertices in the graph")

vertexCount :: (String, String)
vertexCount = ("vertexCount", "Count the vertices of the graph")

hasVertex :: SpecialisedSuite Vertex i
hasVertex genArg = ("hasVertex", "Test if the given vertex is in the graph", map (fmap genArg) . withNames . vertices)
    where
      vertices x = let maxV = extractMaxVertex x
                   in nub $ 0 : maxV `div` 3 : (2 * maxV) `div` 3 : [maxV + 1]

addVertex :: SpecialisedSuite Vertex i
addVertex genArg = ("addVertex", "Add a vertex (not already in the graph)", map (fmap genArg . nameBy ((++)"new vertex: " . show)) . newV)
    where
      newV x = let getNewV = extractMaxVertex x
                in [getNewV + 1, getNewV + 10]

removeVertex :: SpecialisedSuite Vertex i
removeVertex genArg = ("removeVertex", "Remove a vertex of the graph", map (fmap genArg . nameBy ((++)"vertex: " . show)) . oldV)
    where
      oldV x = let getOldV = extractMaxVertex x - 1
                   in nub [if getOldV < 0 then 0 else getOldV, getOldV `div` 2]

-- Edge work

edgeList :: (String, String)
edgeList = ("edgeList","Produce a list of the edges in the graph")

edgeCount :: (String, String)
edgeCount = ("edgeCount","Count the edges of the graph")

hasEdge :: SpecialisedSuite Edge i
hasEdge genArg =
  ("hasEdge"
  , "Test if the given edge is in the graph (with arguments both in the graph and not in the graph (where applicable))"
  , \x -> map (fmap genArg) $ withNames $ getDifferents x ++ take 3 (edgesNotInGraph x)
  )

hasSelfLoop :: SpecialisedSuite Vertex i
hasSelfLoop genArg =
  ("hasSelfLoop"
  , "Test if the given self-loop is in the graph (with arguments both in the graph and not in the graph (where applicable))"
  ,  \x -> let old = extractMaxVertex x
                       in map (fmap genArg) $ withNames $ nub [0, old `div` 2, old+1, old+100]
  )

addEdge :: SpecialisedSuite Edge i
addEdge genArg =
  ( "addEdge"
  , "Add an edge (not already in the graph)"
  , map (fmap genArg) . withNames . take 4 . edgesNotInGraph
  )

removeEdge :: SpecialisedSuite Edge i
removeEdge genArg =
  ( "removeEdge"
  , "Remove an edge of the graph"
  , map (fmap genArg) . withNames . getDifferents
  )

-- Graph work

isEmpty :: (String, String)
isEmpty = ("isEmpty","Test if the graph is empty")

transpose :: (String, String)
transpose = ("transpose","Transpose (invert all the edges) the graph")

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

context :: SpecialisedSuite Edge i
context genArg =
  ( "mergeContext"
  , "Merge a FGL context in the graph"
  , map (fmap genArg) . withNames . const [(0,3),(1,10),(25,3)]
  )

-- Algorithms

dff :: (String, String)
dff = ("dff","Produce a forest, obtained from a DFS (Deep First Search) of each vertex")

topSort :: (String, String)
topSort = ("topSort","Topological sorting of the vertices")

reachable :: SpecialisedSuite Vertex i
reachable genArg =
  ( "reachable"
  , "Produce a list of reachable vertices from a given one"
  , \x -> map (fmap genArg) $ withNames $ nub [0, extractMaxVertex x]
  )

-- Utils

-- | Take the first, the middle and the last edges, if possible
getDifferents :: Edges -> Edges
getDifferents edgs = if length edgs >= 2
                        then nub [head edgs, edgs !! (length edgs `div` 2 - 1), last $ init edgs]
                        else edgs
