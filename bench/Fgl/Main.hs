import Criterion.Main (defaultMain)
import Weigh (mainWith)

import Fgl.PatriciaTree (functions)

import BenchGraph (allBenchs,allWeighs)

main :: IO ()
main = do
  defaultMain $ allBenchs functions
  mainWith $ allWeighs functions
