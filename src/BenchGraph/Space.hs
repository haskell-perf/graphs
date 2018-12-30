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
import BenchGraph.Suites (Suite (..))
import BenchGraph.Types

-- | Main function, will benchmark the given suite against the given graphs
weigh :: (GraphImpl g, NFData g) => [(GenericGraph, [Size])] -> Suite g -> Weigh ()
weigh graphs' (Suite sname algo inputs') = wgroup (show sname) cases
  where
    cases = mapM_ (uncurry mkGroup) graphs'
    mkGroup (gname, gfunc) ss = wgroup gname $ mapM_ (weighSuite algo inputs' gfunc) ss

weighSuite :: (GraphImpl g, NFData g, NFData o)
           => (i -> g -> o) -> (Edges -> [Named i]) -> (Size -> (Edges,Int)) -> Size -> Weigh ()
weighSuite algorithm' inputs' gfunc size = wgroup (show sizeName) cases
  where
    (edges,sizeName) = gfunc size
    graph = case edges of
              [] -> mkVertex
              edgs -> mkGraph edgs
    cases = mapM_ (uncurry wFunc) $ inputs' edges
    wFunc n i = func n (algorithm' i) $!! graph

allWeigh :: (GraphImpl g, NFData g) => Suite g -> Weigh ()
allWeigh = weigh (graphs False defaultGr)

-- | Use the list from weighCreationList
weighCreation :: (NFData g)
              => (Edges -> g) -- ^ A graph-creator function, typically from the GraphImpl class
              -> Weigh ()
weighCreation mk = wgroup "creation" $ mapM_ (\(str,((_,grf), ss)) -> wgroup str $ mapM_ (\i -> let (gr',sizeName) = grf i in  wgroup (show sizeName) $ func "" mk gr') ss ) weighCreationList

-- | List of generic graph with their case-name
weighCreationList :: [Named (GenericGraph, [Int])]
weighCreationList = [ (n,t) | t@((n, _), _) <- graphs False defaultGr]
