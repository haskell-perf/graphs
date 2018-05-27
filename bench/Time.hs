{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.List (filter, nub, sortBy)
import Data.Function (on)
import Data.Maybe (mapMaybe, catMaybes)
import Control.Monad (when)

import Criterion (Benchmarkable)
import Criterion.Types (Benchmark (..), Report (..), DataRecord( Analysed ), Config (..), SampleAnalysis (..), Verbosity (..), Regression (..))
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (initializeTime, secs)
import Criterion.Monad (withConfig)

import Statistics.Types (estPoint)

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

import Text.Printf (printf)

import Command
import Types
import Best
import Abstract
import Common

-- We consider Benchmark equality using their name
instance Eq Benchmark where
  (==) = on (==) showBenchName

showBenchName :: Benchmark -> Name
showBenchName (Benchmark n _) = n
showBenchName (BenchGroup n _) = n
showBenchName Environment{}    = error "Cannot show the bench name of an Env"

genReport :: Int
           -- ^ The number of '#' to write
           -> Output
           -- ^ Output options ?
           -> [Named Benchmark]
           -- ^ The list of benchmarks with their library name
           -> IO()
genReport _ _ [] = putStrLn "\nNo data\n"
genReport lev flg arr = mapM_  mapped $ nub arr
  where
    mapped e = do
      res <- toPrint lev (staOut flg) arr $ extract e
      case fmap (fmap (map (fmap getMean))) res of
        Nothing -> return ()
        Just res'' -> when (sumOut flg) $ do
          printBest "was the fastest" res''
          printAbstract "faster" res''

toPrint :: Int -> StaOut -> [Named Benchmark] -> Benchmark -> IO (Maybe (Grouped [Named Report]))
toPrint lev flg arr breport = do
  when (not (null bname) && (flg == Ascii || lev == 2)) pTitle
  case breport of
    (BenchGroup _ (BenchGroup _ (Benchmark{}:_):_)) -> if flg /= Html
      then doGrp
      else do
        pTitle
        putStrLn ""
        res'@(Just (Group res)) <- doGrp
        let ch = mapMaybe tkGroup res :: [[Grouped [Named Report]]]
            results = zipWith (curry toNamed) getNOtherGroups $ map (mapMaybe tkSimple) ch :: [Named [[Named Report]]]
            results' = map (fmap (makeAverage . map (map (fmap getMean))) ) results :: [Named [Named Double]]
        printHtml results' secs
        return res'
    BenchGroup{} -> doGrp
    Benchmark{} -> do
      simples <- mapM (traverse benchmarkWithoutOutput) $ mapMaybe (traverse tkSimpleB) $ here breport
      when (flg == Ascii) $ putStrLn $ "\n" ++ showSimples simples
      return $ Just $ Simple simples
    Environment{} -> error "Not wanted environnement"
  where
    pTitle = putStrLn $ unwords [replicate lev '#',bname]
    doGrp = case nubOtherGroups of
              [] -> do
                when (flg /= Html) $ putStrLn "\nNo data\n"
                return Nothing
              real -> Just . Group . catMaybes <$> mapM (toPrint (lev+1) flg otherGroups . extract) real
    nubOtherGroups = nub otherGroups
    getNOtherGroups = map (showBenchName . extract) nubOtherGroups
    bname = showBenchName breport
    otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here breport
    here e = filter (liftExtract (== e)) arr

-- | Bench only if it is possible
tkSimpleB :: Benchmark -> Maybe Benchmarkable
tkSimpleB (Benchmark _ b) = Just b
tkSimpleB _ = Nothing

-- | Get the childs of a BenchGroup, inserting the name of the library
tkChilds :: Benchmark -> Maybe [Benchmark]
tkChilds (BenchGroup _ childs) = Just childs
tkChilds _ = Nothing

showSimples :: [Named Report] -> String
showSimples arr = TAA.render id id id table
  where
    arr' = sortBy (on compare (liftExtract getMean)) arr
    arrD = map (\(Named _ r) -> [secs $ getMean r, printf "%.3f" $ getRSquare r ]) arr'
    libs = map show arr'
    table = T.Table
      (T.Group T.NoLine $ map T.Header libs)
      (T.Group T.SingleLine [T.Header "Time (Mean)", T.Header "R\178"])
      arrD

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis

-- head ?
getRSquare :: Report -> Double
getRSquare = estPoint . regRSquare . head . anRegress . reportAnalysis

-- | Utilitary, disable the standard output of Criterion
benchmarkWithoutOutput :: Benchmarkable -> IO Report
benchmarkWithoutOutput bm = do
  initializeTime
  withConfig defaultConfig' $ do
    Analysed rpt <- runAndAnalyseOne 0 "function" bm
    return rpt
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
            grList' = nub $ modifyL $ grList size
            todo = case opt of
              Nothing -> grList'
              Just opt' -> case opt' of
                  Only bname -> filter ((==) bname . showBenchName . extract) grList'
                  Part one' two -> let one = one' + 1
                                       per = length grList' `div` two
                                       f   = if one' + 1 == two then id else take (one*per)
                                    in drop ((one-1)*per) $ f grList'
            samples = filter (`elem` todo) $ modifyL $ grList size
        putStrLn $ unlines ["# Compare benchmarks\n","Doing:","\n----",showListN todo,"----"]
        genReport 2 flg samples
  where
    grList size = concatMap (sequence . toNamed) [
     ("Alga (Algebra.Graph)",allBenchs size Alga.Graph.functions ++ benchmarkCreation size Alga.Graph.mk ),
     ("Containers (Data.Graph)",allBenchs size Containers.Graph.functions ++ benchmarkCreation size Containers.Graph.mk),
     ("Fgl (Data.Graph.Inductive.PatriciaTree)", allBenchs size Fgl.PatriciaTree.functions ++ benchmarkCreation size Fgl.PatriciaTree.mk),
     ("Fgl (Data.Graph.Inductive.Tree)", allBenchs size Fgl.Tree.functions ++ benchmarkCreation size Fgl.Tree.mk),
     ("Hash-Graph (Data.HashGraph.Strict)", allBenchs size HashGraph.Gr.functions ++ benchmarkCreation size HashGraph.Gr.mk)]
