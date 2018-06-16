module ListS

where

import Data.List (nubBy)

import qualified Containers.Graph
import qualified Fgl.PatriciaTree

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
  , ("Alga", map Shadow Alga.Graph.functions )
  , ("Fgl", map Shadow Fgl.PatriciaTree.functions)
  , ("Hash-Graph", map Shadow HashGraph.Gr.functions)
  ]

