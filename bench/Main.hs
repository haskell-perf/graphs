import Data.List (sort, sortBy, filter, nub)
import Data.Maybe (mapMaybe)

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

data BReport = Simple String String Report | Group String [BReport]

instance Show BReport where
  show (Simple _ name _) = name
  show (Group name _) = name

instance Ord BReport where
  a <= b = show a <= show b 

instance Eq BReport where
  a == b = show a == show b 

runSingleBenchmark :: String -> Benchmark -> IO BReport
runSingleBenchmark str (Benchmark name benchm) = Simple str name <$> benchmarkWithoutOutput benchm
runSingleBenchmark str (BenchGroup name benchmarks) = Group name <$> mapM (runSingleBenchmark str) benchmarks

genReport :: [BReport] -> String 
genReport todo = "# Compare benchmarks \n" ++ genReport' 2 todo

genReport' :: Int -> [BReport] -> String
genReport' lev arr = unlines $ map toPrint $ nub arr 
  where
    toPrint breport = replicate lev '#' ++ " " ++ show breport ++ "\n" ++ case why breport of
      "" -> "No Data"
      oth -> oth
    why br = case br of
      Simple{} -> (++) "\n" $ unlines $ map (\(a,b,c) -> "* "++ a ++ " : "++ show (getMean c) ++ " s. (Mean)" )$ sortBy (\(_,_,a) (_,_,b) -> getMean a `compare` getMean b) $ tkSimple  $ here br
      Group{} -> genReport' (lev+1) $ concat $ mapMaybe tkList $ here br 
    here e = filter (e==) arr

tkSimple :: [BReport] -> [(String,String,Report)]
tkSimple = mapMaybe (\x -> case x of
                       Simple a b c -> Just (a,b,c)
                       Group _ _ -> Nothing)

tkList :: BReport -> Maybe [BReport]
tkList Simple{} = Nothing
tkList (Group _ b) = Just b

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis

benchmarkWithoutOutput :: Benchmarkable -> IO Report
benchmarkWithoutOutput bm = do
  initializeTime
  withConfig defaultConfig $ do
    Analysed rpt <- runAndAnalyseOne 0 "function" bm
    return rpt

-- | Running a benchmark print many informations on stdout, we don't want that, so we redefine the according functions
-- | Run a single benchmark and analyse its performance (took from Criterion.Internal sources, unmodified)
runAndAnalyseOne :: Int -> String -> Benchmarkable -> Criterion DataRecord
runAndAnalyseOne i desc bm = do
  Measurement _ _ meas <- runOne i desc bm
  analyseOne i desc meas

-- | Analyse a single benchmark (took from Criterion.Internal sources, modified)
analyseOne :: Int -> String -> V.Vector Measured -> Criterion DataRecord
analyseOne i desc meas = do
  erp <- runExceptT $ analyseSample i desc meas
  case erp of
    Left err -> printError "*** Error: %s\n" err
    Right rpt -> return (Analysed rpt)

main :: IO ()
main = do
  let todo = [("Alga",Alga.allBenchs), ("Containers",Containers.allBenchs), ("Fgl", Fgl.allBenchs)]
  allBenchs <- mapM (\(x,y) -> mapM (runSingleBenchmark x) y) todo 
  putStrLn $ genReport $ sort $ concat allBenchs 
  return ()
