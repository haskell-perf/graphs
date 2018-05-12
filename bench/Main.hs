{-# LANGUAGE TupleSections #-}

import Data.List (sortBy, filter, nubBy, uncons)
import Data.Maybe (mapMaybe, isNothing, isJust)
import Control.Monad (unless, void, when)
import System.Environment (getArgs)
import Data.Map.Strict (Map, alter, unionWith, empty, toList)

import Criterion
import Criterion.Types
import Criterion.Internal
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (initializeTime)
import Criterion.Monad (withConfig)

import Statistics.Types

import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified HashGraph.Gr

import BenchGraph (allBenchs, allWeighs)

import Options.Applicative (execParser)
import Command

showBenchmark :: Benchmark -> String
showBenchmark (Benchmark name _) = name
showBenchmark (BenchGroup name _) = name

instance Eq Benchmark where
  a == b = showBenchmark a == showBenchmark b

eq :: (a, Benchmark) -> (b, Benchmark) -> Bool
eq (_,a) (_,b) = a==b

comparesS :: (Ord a) => (b,a) -> (c,a) -> Ordering
comparesS (_,t1) (_,t2) = t1 `compare` t2

data Grouped a = Simple a | Group [Grouped a] deriving (Show)

genReport :: Int
           -- ^ The number of '#' to write
           -> Maybe Flag
           -- ^ Flag ?
           -> [(String,Benchmark)]
           -- ^ The list of benchmarks with their library name
           -> IO()
genReport _ _ [] = putStrLn "\nNo data\n"
genReport lev flg arr = mapM_ (\x -> toPrint lev flg arr (snd x) >>= printMap . getFastest empty) $ nubBy eq arr

toPrint :: Int -> Maybe Flag -> [(String,Benchmark)] -> Benchmark -> IO (Grouped [(String, Double)])
toPrint lev flg arr breport = do
  let name = showBenchmark breport
  unless (null name || (isJust flg && lev /= 2)) $ putStrLn $ replicate lev '#' ++ " " ++ showBenchmark breport
  case breport of
    Benchmark{} -> do
      simples <- sequence $ mapMaybe tkSimple $ here breport
      when (isNothing flg) $ putStrLn $ "\n" ++ showSimples simples
      return $ Simple simples
    BenchGroup{} -> Group <$> mapM (toPrint (lev+1) flg otherGroups . snd) (nubBy eq otherGroups)
  where
    otherGroups = concatMap tkChilds $ here breport
    here e = filter (\(_,b) -> e==b) arr

printMap :: Map String Int -> IO ()
printMap m = do
  putStrLn "\nSUMMARY:"
  void $ foldMap (\(k,v) -> putStrLn $ k ++" was the fastest " ++show v++" times") $ sortBy (flip comparesS) $ toList m
  putStrLn ""

getFastest :: Map String Int -> Grouped [(String,Double)] -> Map String Int
getFastest m (Simple a) = getFastest' (sortBy comparesS a) m
getFastest m (Group grp) = foldr (unionWith (+) . getFastest m) empty grp

getFastest' :: [(String,Double)] -> Map String Int -> Map String Int
getFastest' (l:_) = alter (Just . maybe 1 (+ 1)) $ fst l

-- | Bench only if it is possible
tkSimple :: (String, Benchmark) -> Maybe (IO (String, Double))
tkSimple (libName,Benchmark _ b) = Just $ benchmarkWithoutOutput b >>= \x -> return (libName, getMean x)
tkSimple (_,BenchGroup{}) = Nothing

-- | Get the childs of a BenchGroup, inserting the name of the library
tkChilds :: (String,Benchmark) -> [(String,Benchmark)]
tkChilds (_,Benchmark{}) = []
tkChilds (lib,BenchGroup _ childs) = insertName lib childs

showSimples :: [(String,Double)] -> String
showSimples = unlines . map shw . sortBy comparesS
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
insertName name = map (name,)

showBenchsName :: [(a,Benchmark)] -> String
showBenchsName = unlines . map (showBenchmark . snd)

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy f a = foldr (\x y -> f a x || y) False

main :: IO ()
main = execParser commandI >>= main'

main' :: Command -> IO ()
main' opts
  = case opts of
      List -> putStr $ showBenchsName grList'
      Run opt' flg -> do
          let todo = case opt' of
                Nothing -> grList
                Just opt'' -> case opt'' of
                  Only name -> filter ((==) name . showBenchmark . snd) grList
                  Part one' two -> let one = one' + 1
                                       per = length grList' `div` two
                                   in drop ((one-1)*per) $ take (one*per) grList'
          let samples = filter ((flip $ elemBy eq) todo) grList
          putStrLn "# Compare benchmarks\n"
          putStrLn "Doing:"
          putStrLn $ "\n----\n"++showBenchsName (nubBy eq samples) ++ "----\n"
          genReport 2 flg todo

  where
    grList = concatMap (uncurry insertName) [
     ("Alga (Algebra.Graph)",allBenchs Alga.Graph.functions),
     ("Containers (Data.Graph)",allBenchs Containers.Graph.functions),
     ("Fgl (Data.Graph.Inductive.PatriciaTree)", allBenchs Fgl.PatriciaTree.functions),
     ("Hash-Graph (Data.HashGraph.Strict)", allBenchs HashGraph.Gr.functions)]
    grList' = nubBy eq grList
