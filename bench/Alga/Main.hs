import Criterion.Main (defaultMain)

import qualified Alga.Graph (functions)
import qualified Alga.NonEmptyGraph (functions)

import BenchGraph (allBenchs, allWeighs)
import BenchGraph.Utils (mainWeigh)

main :: IO ()
main = mainWeigh (allWeighs Alga.Graph.functions >> allWeighs Alga.NonEmptyGraph.functions) $
        defaultMain $ allBenchs Alga.Graph.functions ++ allBenchs Alga.NonEmptyGraph.functions
