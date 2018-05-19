import Data.List (filter, nub, sort)
import Data.Maybe (mapMaybe, catMaybes)
import Control.Monad (when, (>=>))

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
import qualified Fgl.Tree

import BenchGraph (allBenchs, benchmarkCreation)
import BenchGraph.Named

import Control.Comonad (extract)

import Options.Applicative (execParser)

import qualified Text.Tabular as T
import qualified Text.Tabular.AsciiArt as TAA

import Command
import Types
import Best

-- We consider Benchmark equality using their name
instance Eq Benchmark where
  a == b = showBenchName a == showBenchName b

showBenchName :: Benchmark -> Name
showBenchName (Benchmark n _) = n
showBenchName (BenchGroup n _) = n

genReport :: Int
           -- ^ The number of '#' to write
           -> Output
           -- ^ Output options ?
           -> [Named Benchmark]
           -- ^ The list of benchmarks with their library name
           -> IO()
genReport _ _ [] = putStrLn "\nNo data\n"
genReport lev flg arr = mapM_ (toPrint lev flg arr . extract >=> maybe (return ()) (when (sumOut flg) . printBest "was the fastest")) $ nub arr

toPrint :: Int -> Output -> [Named Benchmark] -> Benchmark -> IO (Maybe (Grouped [Named Double]))
toPrint lev flg arr breport = do
  let bname = showBenchName breport
  when (not (null bname) && (staOut flg || lev == 2)) $ putStrLn $ replicate lev '#' ++ " " ++ bname
  case breport of
    Benchmark{} -> do
      simples <- mapM (traverse benchmarkWithoutOutput) $ mapMaybe (traverse tkSimple) $ here breport
      when (staOut flg) $ putStrLn $ "\n" ++ showSimples simples
      return $ Just $ Simple simples
    BenchGroup{} -> case nub otherGroups of
                      [] -> putStrLn "\nNo data\n" >> return Nothing
                      real -> Just . Group . catMaybes <$> mapM (toPrint (lev+1) flg otherGroups . extract) real
  where
    otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here breport
    here e = filter (liftExtract (== e)) arr

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
    arr' = sort arr
    arrD = map (show . extract) arr'
    libs = map show arr'
    table = T.Table
      (T.Group T.NoLine $ map T.Header libs)
      (T.Group T.SingleLine [T.Header "Seconds (Mean)"])
      (map return arrD)

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis

-- | Utilitary, disable the standard output of Criterion
benchmarkWithoutOutput :: Benchmarkable -> IO Double
benchmarkWithoutOutput bm = do
  initializeTime
  withConfig defaultConfig' $ do
    Analysed rpt <- runAndAnalyseOne 0 "function" bm
    return $ getMean rpt
  where
    defaultConfig' = defaultConfig {verbosity = Quiet}

-- show a list of benchmarks
showListN :: [Named Benchmark] -> String
showListN = unlines . map (showBenchName . extract)

main :: IO ()
main = execParser commandTime >>= main'

main' :: Command -> IO ()
main' opts
  = case opts of
      List listOpt -> case listOpt of
                        Benchs -> putStr $ showListN $ nub $ grList (0,0,0)
                        Libs -> putStr $ unlines $ nub $ map show $ grList (0,0,0)
      Run opt flg libs size -> do
        let modifyL = case libs of
              Nothing -> id
              Just libss -> filter (\x -> show x `elem` libss)
        let grList' = nub $ modifyL $ grList size
        let todo = case opt of
              Nothing -> grList'
              Just opt' -> case opt' of
                  Only bname -> filter ((==) bname . showBenchName . extract) grList'
                  Part one' two -> let one = one' + 1
                                       per = length grList' `div` two
                                    in drop ((one-1)*per) $ take (one*per) grList'
        let samples = filter (`elem` todo) $ modifyL $ grList size
        putStrLn "# Compare benchmarks\n"
        putStrLn "Doing:"
        putStrLn $ "\n----\n"++ showListN todo ++ "----\n"
        genReport 2 flg samples
  where
    grList size = concatMap (sequence . toNamed) [
     ("Alga (Algebra.Graph)",allBenchs size Alga.Graph.functions ++ benchmarkCreation size Alga.Graph.mk ),
     ("Containers (Data.Graph)",allBenchs size Containers.Graph.functions ++ benchmarkCreation size Containers.Graph.mk),
     ("Fgl (Data.Graph.Inductive.PatriciaTree)", allBenchs size Fgl.PatriciaTree.functions ++ benchmarkCreation size Fgl.PatriciaTree.mk),
     ("Fgl (Data.Graph.Inductive.Tree)", allBenchs size Fgl.Tree.functions ++ benchmarkCreation size Fgl.Tree.mk),
     ("Hash-Graph (Data.HashGraph.Strict)", allBenchs size HashGraph.Gr.functions ++ benchmarkCreation size HashGraph.Gr.mk)]
