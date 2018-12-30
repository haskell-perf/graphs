{-# LANGUAGE CPP #-}

module ListS

where

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

-- If you are trying to add your library using YourLib/Graph.hs:
--   Uncomment the corresponding lines and it is ready to be used, unless you want to bench the graph creation.
--   If you want to, go to bench/Time.hs

-- UNCOMMENT import qualified YourLib.Graph

import BenchGraph.Named
import BenchGraph.Suites (ShadowedS (..))

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
-- UNCOMMENT, ("YourFancyLibName", map Shadow YourLib.Graph.functions)
  ]
