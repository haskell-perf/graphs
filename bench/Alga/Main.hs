import Criterion.Main (defaultMain)

import qualified Alga.Graph (functions)

import BenchGraph (allBenchs, allWeighs)
import BenchGraph.Utils (mainWeigh)

main :: IO ()
main = mainWeigh allWeighs Alga.Graph.functions $
        defaultMain $ allBenchs Alga.Graph.functions
