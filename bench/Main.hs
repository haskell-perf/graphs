import Criterion
import Criterion.Types

import qualified Alga
import qualified Containers
import qualified Fgl

data BReport = Simple String Report | Group String [BReport] deriving (Show)

runSingleBenchmark :: Benchmark -> IO BReport
runSingleBenchmark (Benchmark name benchm) = Simple name <$> benchmark' benchm
runSingleBenchmark (BenchGroup name benchmarks) = Group name <$> mapM runSingleBenchmark benchmarks

main :: IO ()
main = do
  bchsAlga <- mapM runSingleBenchmark Alga.allBenchs
  bchsContainers <- mapM runSingleBenchmark Containers.allBenchs
  bchsFgl <- mapM runSingleBenchmark Fgl.allBenchs
  return ()
