module BenchGraph.Space (
  weigh,
  allWeigh,
  weighCreation,
  weighCreationList
) where

import Weigh

import Control.DeepSeq (NFData, ($!!))

import BenchGraph.GenericGraph
import BenchGraph.Utils (graphs, defaultGr)
import BenchGraph.Named
import BenchGraph.Types

-- | Main function, will benchmark the given suite against the given graphs
weigh :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> Suite g -> Weigh ()
weigh graphs' (Suite sname _ algo inputs') = wgroup sname cases
  where
    cases = mapM_ (uncurry mkGroup) graphs'
    mkGroup (gname, gfunc) ss = wgroup gname $ mapM_ (weighSuite algo inputs' gfunc) ss

weighSuite :: (GraphImpl g, NFData g, NFData o)
           => (i -> g -> o) -> (Edges -> [Named i]) -> (Size -> Edges) -> Size -> Weigh ()
weighSuite algorithm' inputs' gfunc size = wgroup (show size) cases
  where
    edges = gfunc size
    graph = case edges of
              [] -> mkVertex
              edgs -> mkGraph edgs
    cases = mapM_ (uncurry wFunc) $ inputs' edges
    wFunc n i = func n (algorithm' i) $!! graph

allWeigh :: (GraphImpl g, NFData g) => Suite g -> Weigh ()
allWeigh = weigh (graphs defaultGr)

-- | Use the list from weighCreationList
weighCreation :: (NFData g)
              => (Edges -> g) -- ^ A graph-creator function, typically from the GraphImpl class
              -> Weigh ()
weighCreation mk = wgroup "creation" $ mapM_ (\(str,((_,grf), ss)) -> wgroup str $ mapM_ (\i -> wgroup (show i) $ func "" mk $ grf i ) ss ) weighCreationList

-- | List of generic graph with their case-name
weighCreationList :: [Named (GenericGraph, [Int])]
weighCreationList = [ (n,t) | t@((n, _), _) <- graphs defaultGr]
