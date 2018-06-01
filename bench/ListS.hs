module ListS

where

import Data.List (nubBy)

import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified HashGraph.Gr

import BenchGraph
import BenchGraph.Named

-- | List of descs
descs :: [Named String]
descs = nubBy eq1 $ map ((\(Shadow s) -> extractDescription s) . snd) listOfSuites

-- | List of queued Suite, "Shadowized"
listOfSuites :: [Named ShadowedS]
listOfSuites = concatMap sequence
  [ ("Alga", map Shadow Alga.Graph.functions )
  , ("Containers", map Shadow Containers.Graph.functions)
  , ("Fgl", map Shadow Fgl.PatriciaTree.functions)
  , ("Hash-Graph", map Shadow HashGraph.Gr.functions)
  ]


