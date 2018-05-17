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

import BenchGraph (allBenchs)
import BenchGraph.Named

import Control.Comonad (extract)

import Options.Applicative (execParser)
import Command

import qualified Text.Tabular as T
import qualified Text.Tabular.AsciiArt as TAA

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
genReport lev flg arr = mapM_ (toPrint lev flg arr . extract >=> (printMap . getFastests)) $ nub arr

toPrint :: Int -> Maybe Flag -> [Named Benchmark] -> Benchmark -> IO (Grouped [Named Double])
toPrint lev flg arr breport = do
  let bname = showBenchName breport
  unless (null bname || (isJust flg && lev /= 2)) $ putStrLn $ replicate lev '#' ++ " " ++ bname
  case breport of
    Benchmark{} -> do
      simples <- mapM (traverse benchmarkWithoutOutput) $ mapMaybe (traverse tkSimple) $ here breport
      when (isNothing flg) $ putStrLn $ "\n" ++ showSimples simples
      return $ Simple simples
    BenchGroup{} -> Group <$> mapM (toPrint (lev+1) flg otherGroups . extract) (nub otherGroups)
  where
    otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here breport
    here e = filter (liftExtract (== e)) arr

printMap :: [Named Int] -> IO ()
printMap m = do
  putStrLn "\nSUMMARY:"
  void $ foldMap (\(Named k v) -> putStrLn $ k ++ " was the fastest " ++ show v ++ " times") m
  putStrLn ""

-- | get fastests libraries, sorted
getFastests :: Grouped [Named Double] -> [Named Int]
getFastests = sortBy (flip compare) . map toNamed . toList . getFastests' empty

getFastests' :: Map String Int -> Grouped [Named Double] -> Map String Int
getFastests' m (Simple a) = alter (Just . maybe 1 (+ 1)) (show $ minimum a) m
getFastests' m (Group grp) = foldr (unionWith (+) . getFastests' m) empty grp

-- | Bench only if it is possible
tkSimple :: Benchmark -> Maybe Benchmarkable
tkSimple (Benchmark _ b) = Just b
tkSimple _ = Nothing

-- | Get the childs of a BenchGroup, inserting the name of the library
tkChilds :: Benchmark -> Maybe [Benchmark]
tkChilds (BenchGroup _ childs) = Just childs
tkChilds _ = Nothing

showSimples :: [Named Double] -> String
showSimples arr = TAA.render id id id table
  where
    arrD = map (show . extract) arr
    libs = map show arr
    table = T.Table
      (T.Group T.NoLine $ map T.Header libs)
      (T.Group T.SingleLine [T.Header "Seconds (Mean)"])
      (map return arrD)

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis

benchmarkWithoutOutput :: Benchmarkable -> IO Double
benchmarkWithoutOutput bm = do
  initializeTime
  withConfig defaultConfig' $ do
    Analysed rpt <- runAndAnalyseOne 0 "function" bm
    return $ getMean rpt
  where
    defaultConfig' = defaultConfig {verbosity = Quiet}

showListN :: [Named Benchmark] -> String
showListN = unlines . map (showBenchName . extract)

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
                  Only bname -> filter ((==) bname . showBenchName . extract) grList'
                  Part one' two -> let one = one' + 1
                                       per = length grList' `div` two
                                   in drop ((one-1)*per) $ take (one*per) grList'
          let samples = filter (`elem` todo) grList
          putStrLn "# Compare benchmarks\n"
          putStrLn "Doing:"
          putStrLn $ "\n----\n"++ showListN todo ++ "----\n"
          genReport 2 flg samples

  where
    grList = concatMap (sequence . toNamed) [
     ("Alga (Algebra.Graph)",allBenchs Alga.Graph.functions),
     ("Containers (Data.Graph)",allBenchs Containers.Graph.functions),
     ("Fgl (Data.Graph.Inductive.PatriciaTree)", allBenchs Fgl.PatriciaTree.functions),
     ("Hash-Graph (Data.HashGraph.Strict)", allBenchs HashGraph.Gr.functions)]
    grList' = nub grList
