import Data.List (sortBy, filter, nubBy)
import Data.Maybe (mapMaybe)
import Control.Monad (unless)
import System.Environment (getArgs)

import Criterion
import Criterion.Types
import Criterion.Internal
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (initializeTime)
import Criterion.Monad (withConfig)

import Statistics.Types

import qualified Alga.Graph
import qualified Alga.NonEmptyGraph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified HashGraph.Gr

import BenchGraph (allBenchs, allWeighs)

showBenchmark :: Benchmark -> String
showBenchmark (Benchmark name _) = name
showBenchmark (BenchGroup name _) = name

instance Eq Benchmark where
  a == b = showBenchmark a == showBenchmark b

genReport :: [(String,Benchmark)] -> IO()
genReport todo = do
  putStrLn "# Compare benchmarks"
  genReport' 2 todo

genReport' :: Int
           -- ^ The number of '#' to write
           -> [(String,Benchmark)]
           -- ^ The list of benchmarks with their library name
           -> IO()
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

-- | Bench only if it is possible
tkSimple :: (String, Benchmark) -> Maybe (IO (String, Double))
tkSimple (libName,Benchmark _ b) = Just $ benchmarkWithoutOutput b >>= \x -> return (libName, getMean x)
tkSimple (_,BenchGroup{}) = Nothing

-- | Get the childs of a BenchGroup, inserting the name of the library
tkChilds :: (String,Benchmark) -> [(String,Benchmark)]
tkChilds (_,Benchmark{}) = []
tkChilds (lib,BenchGroup _ childs) = insertName lib childs

showSimples :: [(String,Double)] -> String
showSimples = unlines . map shw . sortBy (\(_,t1) (_,t2) -> t1 `compare` t2)
  where
    shw (libname, time) = "* " ++ libname ++ " : " ++ show time ++ " s. (Mean)"

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis

benchmarkWithoutOutput :: Benchmarkable -> IO Report
benchmarkWithoutOutput bm = do
  initializeTime
  withConfig defaultConfig' $ do
    Analysed rpt <- runAndAnalyseOne 0 "function" bm
    return rpt
  where
    defaultConfig' = defaultConfig {verbosity = Quiet}

insertName :: String -> [i] -> [(String,i)]
insertName name = map (\x -> (name, x))

main :: IO ()
main = do
  args <- getArgs
  if null args
     then genReport grList
     else genReport $ filter ((==) (head args) . showBenchmark . snd) grList
  where
    grList = concatMap (uncurry insertName) [
     ("Alga (Algebra.Graph)",allBenchs Alga.Graph.functions),
     ("Alga (Algebra.Graph.NonEmpty)",allBenchs Alga.NonEmptyGraph.functions),
     ("Containers (Data.Graph)",allBenchs Containers.Graph.functions),
     ("Fgl (Data.Graph.Inductive.PatriciaTree)", allBenchs Fgl.PatriciaTree.functions),
     ("Hash-Graph (Data.HashGraph.Strict)", allBenchs HashGraph.Gr.functions)]
