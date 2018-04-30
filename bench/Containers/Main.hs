import Criterion.Main (defaultMain)
import Weigh (mainWith)

import Containers.Graph (functions)

import BenchGraph (allBenchs,allWeighs)

main :: IO ()
main = do
  defaultMain $ allBenchs functions
  mainWith $ allWeighs functions
