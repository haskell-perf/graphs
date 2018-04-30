import Criterion.Main (defaultMain)
import Weigh (mainWith)

import qualified Alga.Graph (functions)
import qualified Alga.NonEmptyGraph (functions)

import BenchGraph (allBenchs, allWeighs)

main :: IO ()
main = do
  defaultMain $ allBenchs Alga.Graph.functions ++ allBenchs Alga.NonEmptyGraph.functions
  mainWith $ allWeighs Alga.Graph.functions
  mainWith $ allWeighs Alga.NonEmptyGraph.functions
