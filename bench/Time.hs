{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

import Data.List (filter, nub, sortBy, nubBy)
import Data.Function (on)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Control.Monad (when, unless, forM_)
import Data.Bifunctor (second)
import Data.Either (partitionEithers)

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

import Data.Aeson (encodeFile, decodeFileStrict)

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
import BenchGraph.Render.Result
import BenchGraph.Render.Best
import BenchGraph.Render.Abstract
import BenchGraph.Render.Common

#ifdef CHART
import BenchGraph.Render.Chart
#endif

-- We consider Benchmark equality using their name
instance Eq Benchmark where
  (==) = on (==) showBenchName

showBenchName :: Benchmark -> Name
showBenchName (Benchmark n _)  = n
showBenchName (BenchGroup n _) = n
showBenchName Environment{}    = error "Cannot show the bench name of an Env"

genReport :: [(String,Int)]
          -> Output
          -- ^ Output options
          -> [Named (Either (Named String) Benchmark)]
          -- ^ The list of benchmarks with their library name
          -> IO()
genReport _ _ [] = putStrLn "\nNo data\n"
genReport gr flg arr = do
  unless notquickComp $ putStrLn $ let comp = head libNames
                                       oth =  head $ tail libNames
                                   in unwords ["\nComparing",comp,"to",oth,". It means that the displayed number will be k such that", comp,"= k *", oth ]
  results <- fmap catMaybes $ mapM mapped $ nubBy (liftExtract2 (==)) refinedarr
  maybe (return ()) (\x -> encodeFile x $ Result gr results) $ saveToFile flg
  case figOut flg of
    Nothing -> return ()
    (Just x) -> renderG gr x  results
  where
    mapped e = do
      let bname = showBenchName $ snd e
      if notquickComp
        then do
          putStrLn $ unwords [replicate 2 '#',bname]
          maybe (return ()) (putStrLn . (++) "\nDescription: ") (lookup bname descs)
          putStrLn ""
        else putStr $ bname ++ ": "
      res <- toPrint (staOut flg) refinedarr $ snd e
      forM_ (filter (\(_,(a,_)) -> a == showBenchName (snd e)) noimpl) $ \no -> putStrLn $ unwords ["Not implemented for",fst no,"because",snd (snd no)] ++ "."
      case fmap (fmap (map (fmap (\x -> (getCriterionTime x, getStdDev x))))) res of
        Nothing -> return Nothing
        Just res' -> do
          let onlyLargeBenchs = removeTailLast res'
              removeStdDev = fmap (fmap (fmap fst))
          when (sumOut flg) $ if notquickComp
            then do
              printBest "was the fastest" $ removeStdDev res'
              printAbstract "faster" $ removeStdDev onlyLargeBenchs
            else printQuick (head libNames) $ removeStdDev onlyLargeBenchs
          return $ Just (bname,onlyLargeBenchs)
    libNames = nub $ map fst arr
    notquickComp = staOut flg /= QuickComparison
    (noimpl,refinedarr) = partitionEithers $ map stripOutEither arr

renderG :: [(String,Int)] -> ChartOutput -> [(Name, Grouped [(Name, (Double, Double))])] -> IO ()
#ifdef CHART
renderG gr x results = mkChart "Time results" gr secs x $ Right $ sortBy (on compare fst) results
#else
renderG _ _ _ = return ()
#endif

-- | First stage
toPrint :: StaOut -> [Named Benchmark] -> Benchmark -> IO (Maybe (Grouped [Named Report]))
toPrint flg arr breport = doGrp flg toPrint1 $ getOtherGroups breport arr

-- | Second stage
toPrint1 :: StaOut -> [Named Benchmark] -> Benchmark -> IO (Maybe (Grouped [Named Report]))
toPrint1 flg arr breport = do
    when (flg == Ascii || flg == Html) $ putStrLn $ unwords ["###",showBenchName breport]
    res'@(Just (Group res)) <- grp
    let ch = mapMaybe tkGroup res :: [[Grouped [Named Report]]]
        results = reverse $ zip getNOtherGroups $ reverse $ map (mapMaybe tkSimple) ch :: [Named [[Named Report]]] -- Double reverse is necessary, since it can lack some data in the front of ch
        results' = map (fmap (makeAverage . map (map (fmap getCriterionTime))) ) results :: [Named [Named Double]]
    when (flg == Html) $ printHtml results' secs
    return res'
  where
    grp = doGrp flg (toPrint2 0) otherGroups
    getNOtherGroups = reverse $ map (showBenchName . snd) $ nubBy (liftExtract2 (==)) otherGroups
    otherGroups = getOtherGroups breport arr

-- | Last stage
toPrint2 :: Int -> StaOut -> [Named Benchmark] -> Benchmark -> IO (Maybe (Grouped [Named Report]))
toPrint2 lev flg arr breport = do
    when (not (null $ showBenchName breport) && flg == Ascii) $
      putStrLn $ unwords [replicate (4+lev) '#',showBenchName breport]
    case breport of
      BenchGroup{} -> doGrp flg (toPrint2 (lev+1)) $ getOtherGroups breport arr
      Benchmark{} -> do
        simples <- mapM (traverse benchmarkWithoutOutput) $ getOtherSimple breport arr
        when (flg == Ascii) $ putStrLn $ "\n" ++ showSimples simples
        return $ Just $ Simple "" simples -- False by default, changed after
      Environment{} -> error "Not wanted environnement"

getOtherGroups :: Benchmark -> [Named Benchmark] -> [Named Benchmark]
getOtherGroups breport = concatMap sequence . mapMaybe (traverse tkChilds) . filter ((== breport) . snd)

getOtherSimple :: Benchmark -> [Named Benchmark] -> [Named Benchmarkable]
getOtherSimple breport = mapMaybe (traverse tkSimpleB) . filter ((== breport) . snd)

doGrp :: StaOut
      -> (StaOut -> [Named Benchmark] -> Benchmark -> IO (Maybe (Grouped [Named Report])))
      -> [Named Benchmark]
      -> IO (Maybe (Grouped [Named Report]))
doGrp flg f otherGroups =
  case nubBy (liftExtract2 (==)) otherGroups of
    [] -> do
     when (flg == Ascii) $ putStrLn "\nNo data\n"
     return Nothing
    real -> do
     grp <- catMaybes <$> mapM (f flg otherGroups . snd) real
     return $ Just $ Group grp

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

getStdDev :: Report -> Double
getStdDev = estPoint . anStdDev . reportAnalysis

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
main = execParser commandP >>= main'

main' :: Command -> IO ()
main' opts
  = case opts of
      List listOpt -> case listOpt of
                        Benchs -> putStr $ unlines grNames
                        Libs -> putStr $ unlines $ nub $ map fst listOfSuites ++ map fst (listOfCreation False [])
      Render filep dg -> do
        readed <- decodeFileStrict filep
        case readed of
          Nothing -> error "Malformed file"
          Just (Result gr res) -> renderG gr dg res
      Run opt nottodo' flg libs benchWithCreation dontBenchLittleOnes gr' -> do
        let modifyL = case libs of
              Nothing -> id
              Just libss -> filter (\x -> fst x `elem` libss)
            gr = mkGr gr'
            grList' = modifyL $ grList benchWithCreation dontBenchLittleOnes gr
            nottodo = fromMaybe [] nottodo'
            todo = case opt of
              Nothing -> grNames
              Just opt' -> case opt' of
                  Only bname -> bname
                  Part one' two -> let one = one' + 1
                                       per = length grNames `div` two
                                       f   = if one' + 1 == two then id else take (one*per)
                                    in drop ((one-1)*per) $ f grNames
            samples = filter (\(_,n) -> let nam = either fst showBenchName n in nam `elem` todo && nam `notElem` nottodo) grList'
        unless (staOut flg == QuickComparison) $ printHeader gr $ nub $ map (either fst showBenchName . snd) samples
        genReport gr flg samples
  where
    grNames = nub $ map (either fst showBenchName . snd) $ grList False False defaultGr
    grList benchWithCreation dontBenchLittleOnes gr = sortBy (on compare (either fst showBenchName . snd)) $
      map (fmap (\(Shadow s) -> second (allBench benchWithCreation dontBenchLittleOnes gr) s)) listOfSuites
      ++ listOfCreation dontBenchLittleOnes gr
    mkGr gr' = case gr' of
                 [] -> defaultGr
                 g -> g

-- Note: The layout of the list is important
listOfCreation :: Bool -> [(String,Int)] -> [Named (Either (String,String) Benchmark)]
listOfCreation dontBenchLittleOnes gr =
  [ ("Containers", Right $ benchmarkCreation dontBenchLittleOnes gr Containers.Graph.mk)
#ifdef ALGA
  , ("Alga", Right $ benchmarkCreation dontBenchLittleOnes gr Alga.Graph.mk )
#endif
#ifdef FGL
  , ("Fgl", Right $ benchmarkCreation dontBenchLittleOnes gr Fgl.PatriciaTree.mk)
#endif
#ifdef HASHGRAPH
  , ("Hash-Graph", Right $ benchmarkCreation dontBenchLittleOnes gr HashGraph.Gr.mk)
#endif
-- UNCOMMENT, ("YourFancyLibName", benchmarkCreation dontBenchLittleOnes gr YourLib.Graph.mk)
  ]

