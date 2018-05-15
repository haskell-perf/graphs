import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified HashGraph.Gr

import BenchGraph (allWeighs)
import BenchGraph.Utils (mainWeigh)

import Weigh

useResults :: [Grouped (Weight, Maybe String)] -> IO ()
useResults res = print res

main :: IO ()
main = mainWeigh benchs useResults
  where
    benchs = do
      wgroup "Alga (Algebra.Graph)" $ allWeighs Alga.Graph.functions
      wgroup "Containers (Data.Graph)" $ allWeighs Containers.Graph.functions
      wgroup "Fgl (Data.Graph.Inductive.PatriciaTree)" $ allWeighs Fgl.PatriciaTree.functions
      wgroup "Hash-Graph (Data.HashGraph.Strict)" $ allWeighs HashGraph.Gr.functions
