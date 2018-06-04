{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.List (filter, nub, sortBy, nubBy)
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

import BenchGraph (allBench, benchmarkCreation, ShadowedS (..) )
import BenchGraph.Named
import BenchGraph.Utils (defaultGr)

import Options.Applicative (execParser)

import qualified Text.Tabular as T
import qualified Text.Tabular.AsciiArt as TAA

import Text.Printf (printf)

import Command
import ListS (listOfSuites, descs)

import BenchGraph.Render.Types
import BenchGraph.Render.Best
import BenchGraph.Render.Abstract
import BenchGraph.Render.Common

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
genReport lev flg arr = mapM_  mapped $ nubBy (liftExtract2 (==)) arr
  where
    mapped e = do
      res <- toPrint lev (staOut flg) arr $ snd e
      case fmap (fmap (map (fmap getMean))) res of
        Nothing -> return ()
        Just res'' -> when (sumOut flg) $ do
          printBest "was the fastest" res''
          printAbstract "faster" res''

toPrint :: Int -- ^ Must start with 2
        -> StaOut -> [Named Benchmark] -> Benchmark -> IO (Maybe (Grouped [Named Report]))
toPrint lev flg arr breport = case lev of
  2 -> do
    pTitle
    maybe (return ()) (putStrLn . (++) "\nDescritpion: ") (lookup bname descs)
    putStrLn ""
    doGrp
  3 -> do
    pTitle
    if flg /= Html
       then doGrp
       else do
         res'@(Just (Group res)) <- doGrp
         let ch = mapMaybe tkGroup res :: [[Grouped [Named Report]]]
             results = reverse $ zip getNOtherGroups $ reverse $ map (mapMaybe tkSimple) ch :: [Named [[Named Report]]] -- Double reverse is necessary, since it can lack some data in the front of ch
             results' = map (fmap (makeAverage . map (map (fmap getMean))) ) results :: [Named [Named Double]]
         printHtml results' secs
         return res'
  _ -> do
    when (not (null bname) && flg == Ascii) pTitle
    case breport of
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
              real -> Just . Group . catMaybes <$> mapM (toPrint (lev+1) flg otherGroups . snd) real
    nubOtherGroups = nubBy (liftExtract2 (==)) otherGroups
    getNOtherGroups = reverse $ map (showBenchName . snd) nubOtherGroups
    bname = showBenchName breport
    otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here breport
    here e = filter ((== e) . snd) arr

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
    arr' = sortBy (on compare (getMean . snd)) arr
    arrD = map (\(_,r) -> [secs $ getMean r, printf "%.3f" $ getRSquare r ]) arr'
    libs = map fst arr'
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

main :: IO ()
main = execParser commandTime >>= main'

main' :: Command -> IO ()
main' opts
  = case opts of
      List listOpt -> case listOpt of
                        Benchs -> putStr $ unlines grNames
                        Libs -> putStr $ unlines $ nub $ map fst $ grList []
      Run opt flg libs gr' -> do
        let modifyL = case libs of
              Nothing -> id
              Just libss -> filter (\x -> fst x `elem` libss)
            gr = mkGr gr'
            grList' = modifyL $ grList gr
            todo = case opt of
              Nothing -> grNames
              Just opt' -> case opt' of
                  Only bname -> bname
                  Part one' two -> let one = one' + 1
                                       per = length grNames `div` two
                                       f   = if one' + 1 == two then id else take (one*per)
                                    in drop ((one-1)*per) $ f grNames
            samples = filter (\(_,n) -> showBenchName n `elem` todo) grList'
        printHeader gr $ nub $ map (showBenchName . snd) samples
        genReport 2 flg samples
  where
    grNames = nub $ map (showBenchName . snd) $ grList defaultGr
    grList gr = map (fmap (\(Shadow s) -> allBench gr s)) listOfSuites ++ listOfCreation gr
    mkGr gr' = case gr' of
                 [] -> defaultGr
                 g -> g

listOfCreation :: [(String,Int)] -> [Named Benchmark]
listOfCreation gr =
  [ ("Alga", benchmarkCreation gr Alga.Graph.mk )
  , ("Containers", benchmarkCreation gr Containers.Graph.mk)
  , ("Fgl", benchmarkCreation gr Fgl.PatriciaTree.mk)
  , ("Hash-Graph", benchmarkCreation gr HashGraph.Gr.mk)
  ]

