{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

import Data.List (filter, nub, sortBy, nubBy)
import Data.Function (on)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Control.Monad (when, unless)

import Criterion (Benchmarkable)
import Criterion.Types (Benchmark (..), Report (..), DataRecord( Analysed ), Config (..), SampleAnalysis (..), Verbosity (..), Regression (..))
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (initializeTime, secs)
import Criterion.Monad (withConfig)

import qualified Data.Map as Map

import Statistics.Types (estPoint)

import BenchGraph.Types (ShadowedS (..))
import BenchGraph.Time (allBench, benchmarkCreation)
import BenchGraph.Named
import BenchGraph.Utils (defaultGr)

import Options.Applicative (execParser)

import qualified Text.Tabular as T
import qualified Text.Tabular.AsciiArt as TAA

import Text.Printf (printf)

import Command
import ListS (listOfSuites, descs)

-- If you are trying to add your library using YourLib/Graph.hs:
--   Uncomment the corresponding lines, and you are ready to build and run

-- UNCOMMENT import qualified YourLib.Graph

import qualified Containers.Graph
#ifdef ALGA
import qualified Alga.Graph
#endif
#ifdef FGL
import qualified Fgl.PatriciaTree
#endif
#ifdef HASHGRAPH
import qualified HashGraph.Gr
#endif

import BenchGraph.Render.Types
import BenchGraph.Render.Best
import BenchGraph.Render.Abstract
import BenchGraph.Render.Chart
import BenchGraph.Render.Common

-- We consider Benchmark equality using their name
instance Eq Benchmark where
  (==) = on (==) showBenchName

showBenchName :: Benchmark -> Name
showBenchName (Benchmark n _) = n
showBenchName (BenchGroup n _) = n
showBenchName Environment{}    = error "Cannot show the bench name of an Env"

genReport :: Output
           -- ^ Output options
           -> [Named Benchmark]
           -- ^ The list of benchmarks with their library name
           -> IO()
genReport _ [] = putStrLn "\nNo data\n"
genReport flg arr = do
  unless notquickComp $ putStrLn $ let comp = head libNames
                                       oth =  head $ tail libNames
                                   in unwords ["\nComparing",comp,"to",oth,". It means that the displayed number will be k such that", comp,"= k *", oth ]
  mapM_ mapped $ nubBy (liftExtract2 (==)) arr
  where
    mapped e = do
      let bname = showBenchName $ snd e
      if notquickComp
        then do
          putStrLn $ unwords [replicate 2 '#',bname]
          maybe (return ()) (putStrLn . (++) "\nDescription: ") (lookup bname descs)
          putStrLn ""
        else putStr $ bname ++ ": "
      res <- toPrint 2 (staOut flg) arr $ snd e
      case fmap (fmap (map (fmap getCriterionTime))) res of
        Nothing -> return ()
        Just res' -> do
          when (sumOut flg) $ if notquickComp
            then do
              printBest "was the fastest" res'
              printAbstract "faster" $ setBGroupT res'
            else printQuick (head libNames) $ setBGroupT res'
          when (figOut flg) $ mkChart bname res'
    libNames = nub $ map fst arr
    notquickComp = staOut flg /= QuickComparison

toPrint :: Int -- ^ Will start with 2
        -> StaOut -> [Named Benchmark] -> Benchmark -> IO (Maybe (Grouped [Named Report]))
toPrint lev flg arr breport = case lev of
  2 -> doGrp
  3 -> do
    when (flg == Ascii || flg == Html) pTitle
    if flg /= Html
       then doGrp
       else do
         res'@(Just (Group res)) <- doGrp
         let ch = mapMaybe tkGroup res :: [[Grouped [Named Report]]]
             results = reverse $ zip getNOtherGroups $ reverse $ map (mapMaybe tkSimple) ch :: [Named [[Named Report]]] -- Double reverse is necessary, since it can lack some data in the front of ch
             results' = map (fmap (makeAverage . map (map (fmap getCriterionTime))) ) results :: [Named [Named Double]]
         printHtml results' secs
         return res'
  _ -> do
    when (not (null bname) && flg == Ascii) pTitle
    case breport of
      BenchGroup{} -> doGrp
      Benchmark{} -> do
        simples <- mapM (traverse benchmarkWithoutOutput) $ mapMaybe (traverse tkSimpleB) $ here breport
        when (flg == Ascii) $ putStrLn $ "\n" ++ showSimples simples
        return $ Just $ Simple False simples -- False by default, changed after
      Environment{} -> error "Not wanted environnement"
  where
    pTitle = putStrLn $ unwords [replicate lev '#',bname]
    doGrp = case nubOtherGroups of
              [] -> do
                when (flg == Ascii) $ putStrLn "\nNo data\n"
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
    arr' = sortBy (on compare (getCriterionTime . snd)) arr
    arrD = map (\(_,r) -> [secs $ getCriterionTime r, printf "%.3f" $ getRSquare r ]) arr'
    libs = map fst arr'
    table = T.Table
      (T.Group T.NoLine $ map T.Header libs)
      (T.Group T.SingleLine [T.Header "Time", T.Header "R\178"])
      arrD

-- Return the regressed time or the mean if the other is not avaible
getCriterionTime :: Report -> Double
getCriterionTime t = estPoint $ lReg $ regCoeffs $ head $ anRegress $ reportAnalysis t
  where
    lReg = fromMaybe (anMean $ reportAnalysis t) . Map.lookup "iters"

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
    defaultConfig' = defaultConfig {verbosity = Quiet, timeLimit = 10}

main :: IO ()
main = execParser commandTime >>= main'

main' :: CommandTime -> IO ()
main' opts
  = case opts of
      List listOpt -> case listOpt of
                        Benchs -> putStr $ unlines grNames
                        Libs -> putStr $ unlines $ nub $ map fst listOfSuites ++ map fst (listOfCreation False [])
      Run opt flg libs benchWithCreation dontBenchLittleOnes gr' -> do
        let modifyL = case libs of
              Nothing -> id
              Just libss -> filter (\x -> fst x `elem` libss)
            gr = mkGr gr'
            grList' = modifyL $ grList benchWithCreation dontBenchLittleOnes gr
            todo = case opt of
              Nothing -> grNames
              Just opt' -> case opt' of
                  Only bname -> bname
                  Part one' two -> let one = one' + 1
                                       per = length grNames `div` two
                                       f   = if one' + 1 == two then id else take (one*per)
                                    in drop ((one-1)*per) $ f grNames
            samples = filter (\(_,n) -> showBenchName n `elem` todo) grList'
        unless (staOut flg == QuickComparison) $ printHeader gr $ nub $ map (showBenchName . snd) samples
        genReport flg samples
  where
    grNames = nub $ map (showBenchName . snd) $ grList False False defaultGr
    grList benchWithCreation dontBenchLittleOnes gr = map (fmap (\(Shadow s) -> allBench benchWithCreation dontBenchLittleOnes gr s)) listOfSuites ++ listOfCreation dontBenchLittleOnes gr
    mkGr gr' = case gr' of
                 [] -> defaultGr
                 g -> g

-- Note: The layout of the list is important
listOfCreation :: Bool -> [(String,Int)] -> [Named Benchmark]
listOfCreation dontBenchLittleOnes gr =
  [ ("Containers", benchmarkCreation dontBenchLittleOnes gr Containers.Graph.mk)
#ifdef ALGA
  , ("Alga", benchmarkCreation dontBenchLittleOnes gr Alga.Graph.mk )
#endif
#ifdef FGL
  , ("Fgl", benchmarkCreation dontBenchLittleOnes gr Fgl.PatriciaTree.mk)
#endif
#ifdef HASHGRAPH
  , ("Hash-Graph", benchmarkCreation dontBenchLittleOnes gr HashGraph.Gr.mk)
#endif
-- UNCOMMENT, ("YourFancyLibName", benchmarkCreation dontBenchLittleOnes gr YourLib.Graph.mk)
  ]

