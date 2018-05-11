{-# LANGUAGE TupleSections #-}

import Data.List (sortBy, filter, nubBy, uncons, intersectBy)
import Data.Maybe (mapMaybe, maybe)
import Control.Monad (unless, void)
import System.Environment (getArgs)
import Data.Map.Strict (Map, alter, unionWith, empty, foldMapWithKey)

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
genReport' lev arr = mapM_ (\x -> toPrint lev arr (snd x) >>= printMap . getFastest empty) $ nubBy eq arr

toPrint :: Int -> [(String,Benchmark)] -> Benchmark -> IO (Grouped [(String, Double)])
toPrint lev arr breport = do
  let name = showBenchmark breport
  unless (null name) $ putStrLn $ replicate lev '#' ++ " " ++ showBenchmark breport
  case breport of
    Benchmark{} -> do
      simples <- sequence $ mapMaybe tkSimple $ here breport
      putStrLn $ "\n" ++ showSimples simples
      return $ Simple simples
    BenchGroup{} -> Group <$> mapM (toPrint (lev+1) otherGroups . snd) (nubBy eq otherGroups)
  where
    otherGroups = concatMap tkChilds $ here breport
    here e = filter (\(_,b) -> e==b) arr

printMap :: Map String Int -> IO ()
printMap m = do
  putStrLn "\nSUMMARY:"
  void $ foldMap (\k v -> putStrLn $ k ++" was the fastest " ++show v++" times") $ sortBy comparesS $ toList m
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
main = do
  args <- getArgs
  case uncons args of
    Nothing -> genReport grList
    Just (hea,rst) -> case hea of
                      "--list" -> putStr $ showBenchsName grList'
                      "--part" -> case rst of
                        (one:two:_) -> let one' = read one + 1
                                           two' = read two :: Int
                                           per = length grList' `div` two'
                                           todo' = drop ((one'-1)*per) $ take (one'*per) grList'
                                           todo = filter ((flip $ elemBy eq) todo') grList
                                        in do
                                          putStrLn "Doing:"
                                          putStrLn $ "\n----\n"++showBenchsName todo' ++ "----\n"
                                          genReport todo
                        _ -> fail "Malformed option: --part int int"
                      oth -> genReport $ filter ((==) oth . showBenchmark . snd) grList
  where
    grList = concatMap (uncurry insertName) [
     ("Alga (Algebra.Graph)",allBenchs Alga.Graph.functions),
     ("Containers (Data.Graph)",allBenchs Containers.Graph.functions),
     ("Fgl (Data.Graph.Inductive.PatriciaTree)", allBenchs Fgl.PatriciaTree.functions),
     ("Hash-Graph (Data.HashGraph.Strict)", allBenchs HashGraph.Gr.functions)]
    grList' = nubBy eq grList
