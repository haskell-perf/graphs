{-# LANGUAGE CPP #-}

module ListS

where

import Data.List (nubBy)


import qualified Containers.Graph
#ifdef ALGA
import qualified Alga.Graph
#endif
#ifdef FGL
import qualified Fgl.PatriciaTree
#endif
#ifdef HASHGRAPH
import qualified HashGraph.Gr
#endif

import BenchGraph.Types
import BenchGraph.Named

-- | List of descs
descs :: [Named String]
descs = ("creation","Create a graph from a list of edges") : (nubBy eq1 $ map ((\(Shadow s) -> extractDescription s) . snd) listOfSuites)

-- | List of queued Suite, "Shadowized"
-- Note: The layout of the list is important
listOfSuites :: [Named ShadowedS]
listOfSuites = concatMap sequence
  [ ("Containers", map Shadow Containers.Graph.functions)
#ifdef ALGA
  , ("Alga", map Shadow Alga.Graph.functions )
#endif
#ifdef FGL
  , ("Fgl", map Shadow Fgl.PatriciaTree.functions)
#endif
#ifdef HASHGRAPH
  , ("Hash-Graph", map Shadow HashGraph.Gr.functions)
#endif
  ]

