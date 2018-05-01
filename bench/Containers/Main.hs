import Criterion.Main (defaultMain)

import Containers.Graph (functions)

import BenchGraph (allBenchs,allWeighs)
import BenchGraph.Utils (mainWeigh)

main :: IO ()
main = mainWeigh (allWeighs functions) $ defaultMain $ allBenchs functions
