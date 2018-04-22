import Data.List (sortBy, filter, nubBy)
import Data.Maybe (mapMaybe)

import Control.Monad (unless)
import Control.Monad.Except (runExceptT)

import Criterion
import Criterion.Types
import Criterion.Internal (runOne)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (initializeTime)
import Criterion.IO.Printf (printError)
import Criterion.Analysis (analyseSample)
import Criterion.Monad (withConfig, Criterion)
import qualified Data.Vector as V

import Statistics.Types

import qualified Alga
import qualified Containers
import qualified Fgl

showBenchmark :: Benchmark -> String
showBenchmark (Benchmark name _) = name
showBenchmark (BenchGroup name _) = name

instance Eq Benchmark where
  a == b = showBenchmark a == showBenchmark b

genReport :: [(String,Benchmark)] -> IO()
genReport todo = do
  putStrLn "# Compare benchmarks" 
  genReport' 2 todo

genReport' :: Int -> [(String,Benchmark)] -> IO()
genReport' _ [] = putStrLn "\nNo data\n"
genReport' lev arr = mapM_ toPrint $ nubBy (\(_,a) (_,b) -> a == b) arr
  where
    toPrint (_, breport) = do
        let name = showBenchmark breport
        unless (null name) $ putStrLn $ replicate lev '#' ++ " " ++ showBenchmark breport
        case breport of
          Benchmark{} -> do
            simples <- sequence $ mapMaybe tkSimple $ here breport
            putStrLn $ "\n" ++ showSimples simples
          BenchGroup{} -> genReport' (lev+1) $ concatMap tkChilds $ here breport
    here e = filter (\(_,b) -> e==b) arr

tkSimple :: (String, Benchmark) -> Maybe (IO (String, Double))
tkSimple (libName,Benchmark _ b) = Just $ benchmarkWithoutOutput b >>= \x -> return (libName, getMean x)
tkSimple (_,BenchGroup{}) = Nothing

showSimples :: [(String,Double)] -> String
showSimples = unlines . map shw . sortBy (\(_,t1) (_,t2) -> t1 `compare` t2)
  where
    shw (libname, time) = "* " ++ libname ++ " : " ++ show time ++ " s. (Mean)"

tkChilds :: (String,Benchmark) -> [(String,Benchmark)]
tkChilds (_,Benchmark{}) = []
tkChilds (lib,BenchGroup _ childs) = insertName lib childs

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis

benchmarkWithoutOutput :: Benchmarkable -> IO Report 
benchmarkWithoutOutput bm = do
  initializeTime
  withConfig defaultConfig $ do
    Analysed rpt <- runAndAnalyseOne 0 "function" bm
    return rpt

-- | Running a benchmark print many informations on stdout, we don't want that, so we redefine the according functions
-- | Run a single benchmark and analyse its performance (taken from Criterion.Internal sources, unmodified)
runAndAnalyseOne :: Int -> String -> Benchmarkable -> Criterion DataRecord
runAndAnalyseOne i desc bm = do
  Measurement _ _ meas <- runOne i desc bm
  analyseOne i desc meas

-- | Analyse a single benchmark (taken from Criterion.Internal sources, modified)
analyseOne :: Int -> String -> V.Vector Measured -> Criterion DataRecord
analyseOne i desc meas = do
  erp <- runExceptT $ analyseSample i desc meas
  case erp of
    Left err -> printError "*** Error: %s\n" err
    Right rpt -> return (Analysed rpt)

main :: IO ()
main = genReport $ concatMap (uncurry insertName) [("Alga",Alga.allBenchs), ("Containers",Containers.allBenchs), ("Fgl",Fgl.allBenchs)]

insertName :: String -> [i] -> [(String,i)]
insertName name = map (\x -> (name, x))
