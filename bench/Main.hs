import Data.List (sortBy, filter, nub, sort)
import Data.Maybe (mapMaybe, isNothing, isJust)
import Control.Monad (unless, void, when, (>=>))
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
import BenchGraph.Named

import Options.Applicative (execParser)
import Command

instance Eq Benchmark where
  a == b = showBenchName a == showBenchName b

showBenchName :: Benchmark -> Name
showBenchName (Benchmark n _) = n
showBenchName (BenchGroup n _) = n

data Grouped a = Simple a | Group [Grouped a] deriving (Show)

genReport :: Int
           -- ^ The number of '#' to write
           -> Maybe Flag
           -- ^ Flag ?
           -> [Named Benchmark]
           -- ^ The list of benchmarks with their library name
           -> IO()
genReport _ _ [] = putStrLn "\nNo data\n"
genReport lev flg arr = mapM_ (toPrint lev flg arr >=> (printMap . getFastest empty)) $ nub arr

toPrint :: Int -> Maybe Flag -> [Named Benchmark] -> Named Benchmark -> IO (Grouped [Named Double])
toPrint lev flg arr breport = do
  let bname = showBenchName $ obj breport
  unless (null bname || (isJust flg && lev /= 2)) $ putStrLn $ replicate lev '#' ++ " " ++ bname
  case obj breport of
    Benchmark{} -> do
      simples <- sequence $ mapMaybe tkSimple $ here breport
      when (isNothing flg) $ putStrLn $ "\n" ++ showSimples simples
      return $ Simple simples
    BenchGroup{} -> Group <$> mapM (toPrint (lev+1) flg otherGroups) (nub otherGroups)
  where
    otherGroups = concatMap tkChilds $ here breport
    here e = filter (== e) arr

printMap :: Map String Int -> IO ()
printMap m = do
  putStrLn "\nSUMMARY:"
  void $ foldMap (\(Named k v) -> putStrLn $ k ++ " was the fastest " ++ show v ++ " times") $ sortBy (flip compare) $ map toNamed $ toList m
  putStrLn ""

getFastest :: Map String Int -> Grouped [Named Double] -> Map String Int
getFastest m (Simple a) = getFastest' a m
getFastest m (Group grp) = foldr (unionWith (+) . getFastest m) empty grp

getFastest' :: [Named Double] -> Map String Int -> Map String Int
getFastest' l = alter (Just . maybe 1 (+ 1)) $ show $ minimum l

-- | Bench only if it is possible
tkSimple :: Named Benchmark -> Maybe (IO (Named Double))
tkSimple (Named libName (Benchmark _ b)) = Just $ Named libName . getMean <$> benchmarkWithoutOutput b
tkSimple (Named _ BenchGroup{}) = Nothing

-- | Get the childs of a BenchGroup, inserting the name of the library
tkChilds :: Named Benchmark -> [Named Benchmark]
tkChilds (Named _ Benchmark{}) = []
tkChilds (Named lib (BenchGroup _ childs)) = nameChilds lib childs

showSimples :: [Named Double] -> String
showSimples = unlines . map shw . sort
  where
    shw (Named n o) = "* " ++ n ++ " : " ++ show o ++ " s. (Mean)"

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

nameChilds :: String -> [i] -> [Named i]
nameChilds = map . nameBy . const

showListN :: [Named Benchmark] -> String
showListN = unlines . map (showBenchName . obj)

main :: IO ()
main = execParser commandI >>= main'

main' :: Command -> IO ()
main' opts
  = case opts of
      List -> putStr $ showListN grList'
      Run opt flg -> do
          let todo = case opt of
                Nothing -> grList'
                Just opt' -> case opt' of
                  Only bname -> filter ((==) bname . showBenchName . obj) grList'
                  Part one' two -> let one = one' + 1
                                       per = length grList' `div` two
                                   in drop ((one-1)*per) $ take (one*per) grList'
          let samples = filter (`elem` todo) grList
          putStrLn "# Compare benchmarks\n"
          putStrLn "Doing:"
          putStrLn $ "\n----\n"++ showListN todo ++ "----\n"
          genReport 2 flg samples

  where
    grList = concatMap (uncurry nameChilds) [
     ("Alga (Algebra.Graph)",allBenchs Alga.Graph.functions),
     ("Containers (Data.Graph)",allBenchs Containers.Graph.functions),
     ("Fgl (Data.Graph.Inductive.PatriciaTree)", allBenchs Fgl.PatriciaTree.functions),
     ("Hash-Graph (Data.HashGraph.Strict)", allBenchs HashGraph.Gr.functions)]
    grList' = nub grList
