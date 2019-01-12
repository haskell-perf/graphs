{-# LANGUAGE ExistentialQuantification #-}
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

data SuiteT =
    VertexList
  | VertexCount
  | HasVertex
  | AddVertex
  | RemoveVertex
  | EdgeList
  | EdgeCount
  | HasEdge
  | AddEdge
  | RemoveEdge
  | IsEmpty
  | Transpose
  | Equality
  | MergeContext
  | DFF
  | TopSort
  | Reachable
  | Creation
  deriving (Eq,Show, Read)

-- | A graph algorithm operates on a graph type @g@, which takes an input of
-- type @i@ and produces an output of type @o@. Algorithms come with a list of
-- named inputs, all of which will be tried during benchmarking.
data Suite g = forall i o. NFData o => Suite
  { name :: SuiteT
  , algorithm :: i -> g -> o
  , inputs    :: Edges -> [Named i]}

extractDescription :: Suite a -> String
extractDescription x = getDescription (name x)

getDescription :: SuiteT -> String
getDescription x =
  case x of
   VertexList   -> "Produce a list of the vertices in the graph"
   VertexCount  -> "Count the vertices of the graph"
   HasVertex    -> "Test if the given vertex is in the graph"
   AddVertex    -> "Add a vertex (not already in the graph)"
   RemoveVertex ->  "Remove a vertex of the graph"
   EdgeList     -> "Produce a list of the edges in the graph"
   EdgeCount    -> "Count the edges of the graph"
   HasEdge      -> "Test if the given edge is in the graph (with arguments both in the graph and not in the graph (where applicable))"
   AddEdge      -> "Add an edge (not already in the graph)"
   RemoveEdge   -> "Remove an edge of the graph"
   IsEmpty      -> "Test if the graph is empty"
   Transpose    -> "Transpose (invert all the edges) the graph"
   Equality     -> "Test if two graphs are equals"
   MergeContext -> "Merge a FGL context in the graph"
   DFF          -> "Produce a forest, obtained from a DFS (Deep First Search) of each vertex"
   TopSort      -> "Topological sorting of the vertices"
   Reachable    -> "Produce a list of reachable vertices from a given one"
   Creation     -> "Create a graph from a list of edges"

-- A suite that don't take arguments apart a graph
simpleSuite :: NFData o => SuiteT -> (g -> o) -> Suite g
simpleSuite name' algorithm' = Suite name' (const algorithm') (const [("",())])

-- | Type to shadow the argument of a Suite
data ShadowedS = forall g. (GraphImpl g, NFData g) => Shadow (SuiteWithExp g)

type SuiteWithExp g = Either (String,String) (Suite g)

-- | Type to express the common interface between Specialised suites
type SpecialisedSuite u o i g = (i -> g -> o) -- ^ The actual function to test.
  -> (u -> i) -- ^ A function to create the tested function argument
  -> Suite g

-- Vertex work

vertexList :: NFData o => (g -> o) -> Suite g
vertexList = simpleSuite VertexList

vertexCount :: NFData o => (g -> o) -> Suite g
vertexCount = simpleSuite VertexCount

hasVertex :: NFData o => SpecialisedSuite Vertex o i g
hasVertex fun genArg = Suite
  { name = HasVertex
  , algorithm = fun
  , inputs    = map (fmap genArg) . withNames . vertices
  }
    where
      vertices x = let maxV = extractMaxVertex x
                   in nub $ 3 : maxV `div` 3 : (2 * maxV) `div` 3 : [maxV + 1]

addVertex :: NFData o => SpecialisedSuite Vertex o i g
addVertex fun genArg = Suite
  { name = AddVertex
  , algorithm = fun
  , inputs    = map (fmap genArg . nameBy ((++)"new vertex: " . show)) . newV
  }
    where
      newV x = let getNewV = extractMaxVertex x
                in [getNewV + 1, getNewV + 10]

removeVertex :: NFData o => SpecialisedSuite Vertex o i g
removeVertex fun genArg = Suite
  { name = RemoveVertex
  , algorithm = fun
  , inputs    = map (fmap genArg . nameBy ((++)"vertex: " . show)) . oldV
  }
    where
      oldV x = let getOldV = extractMaxVertex x - 1
                   in if getOldV < 0 then [0] else nub [getOldV-3, getOldV `div` 3, 2 * getOldV `div` 3]

-- Edge work

edgeList :: NFData o => (g -> o) -> Suite g
edgeList = simpleSuite EdgeList

edgeCount :: NFData o => (g -> o) -> Suite g
edgeCount = simpleSuite EdgeCount

hasEdge :: NFData o => SpecialisedSuite Edge o i g
hasEdge fun genArg = Suite
  { name = HasEdge
  , algorithm = fun
  , inputs    = \x -> map (fmap genArg) $ withNames $ getDifferents x ++ take 3 (edgesNotInGraph x)
  }

addEdge :: NFData o => SpecialisedSuite Edge o i g
addEdge fun genArg = Suite
  { name = AddEdge
  , algorithm = fun
  , inputs = map (fmap genArg) . withNames . take 3 . edgesNotInGraph
  }

removeEdge :: NFData o => SpecialisedSuite Edge o i g
removeEdge fun genArg = Suite
  { name = RemoveEdge
  , algorithm = fun
  , inputs    = map (fmap genArg) . withNames . getDifferents
  }

-- Graph work

isEmpty :: NFData o => (g -> o) -> Suite g
isEmpty = simpleSuite IsEmpty

transpose :: NFData o => (g -> o) -> Suite g
transpose = simpleSuite Transpose

eq :: (NFData g, GraphImpl g) => (g -> g -> Bool) -> Suite g
eq fun = Suite
  { name = Equality
  , algorithm = fun
  , inputs    = \x -> fmap mkGraph <$>
    [nameBy ((++)"vertex: " . show . head) [(0,2)]
    ,("Same graph",x)
    ]
  }

context :: NFData o => SpecialisedSuite Edge o i g
context fun genArg = Suite
  { name = MergeContext
  , algorithm = fun
  , inputs = map (fmap genArg) . withNames . const [(0,3),(1,10),(25,3)]
  }

-- Algorithms

dff :: NFData o => (g -> o) -> Suite g
dff = simpleSuite DFF

topSort :: NFData o => (g -> o) -> Suite g
topSort = simpleSuite TopSort

reachable :: NFData o => SpecialisedSuite Vertex o i g
reachable fun genArg = Suite
  { name = Reachable
  , algorithm = fun
  , inputs    = \x -> map (fmap genArg) $ withNames $ nub [0, extractMaxVertex x]
  }

-- Utils

-- | Take the first, the middle and the last edges, if possible
getDifferents :: Edges -> Edges
getDifferents edgs =
  if length edgs >= 2
  then nub [edgs!!2, edgs !! (length edgs `div` 2 - 1), last $ init edgs]
  else edgs
