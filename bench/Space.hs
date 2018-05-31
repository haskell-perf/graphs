import Data.List (nub, nubBy, sortBy, elemIndices)
import Data.Function (on)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Int (Int64)
import Control.Monad (when, unless, (>=>))

import Weigh (Grouped (..), Weight (..), Weigh, wgroup, commas)

import qualified Text.Tabular as TA
import qualified Text.Tabular.AsciiArt as TAA

import Options.Applicative (execParser)

import Command

import BenchGraph
import BenchGraph.Named
import BenchGraph.Utils (mainWeigh)

import qualified BenchGraph.Render.Types as T
import BenchGraph.Render.Best
import BenchGraph.Render.Abstract
import BenchGraph.Render.Common

import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified HashGraph.Gr

type WeighResult = (Weight,Maybe String)

showGrouped :: Grouped a -> String
showGrouped (Grouped n _) = n
showGrouped _ = ""

-- | Grouped are equals by their names
eqG :: Grouped a -> Grouped a -> Bool
eqG = on (==) showGrouped

-- | WeighResult are equals by their names
eqW :: WeighResult -> WeighResult -> Bool
eqW = on (==) (takeLastAfterBk . weightLabel . fst)

-- | Drop the prefix of a WeighResult
takeLastAfterBk :: String -> String
takeLastAfterBk w = case elemIndices '/' w of
                          [] -> w
                          x -> drop (1+last x) w

useResults :: Output -> [Grouped WeighResult] -> IO ()
useResults (Output su st) todo = mapM_ mapped $ nubBy (liftExtract2 eqG) namedBenchs
  where
    namedBenchs = concatMap sequence $ mapMaybe groupedToNamed todo
    mapped e = do
      res <- printReport 2 st namedBenchs $ snd e
      case res of
        Nothing -> return ()
        Just res' ->
          let res'' = fmap (fmap (fmap (fromRational . toRational))) res'
              in when su $ do
                printBest "used the least amount of memory" res''
                printAbstract "lighter" res''

-- | Print a report from the lists of benchmarks
printReport :: Int -- ^ The number of # to write
            -> StaOut -- ^ Output infos
            -> [Named (Grouped WeighResult)] -- ^ The list of benchs
            -> Grouped WeighResult -- ^ A selected bench name
            -> IO (Maybe (T.Grouped [Named Int64])) -- Maybe if there was actual data
printReport lev flg arr act = do
  when (not (null bname) && (flg == Ascii || lev == 2)) pTitle
  case act of
    (Grouped _ (Grouped _ (Singleton{}:_):_)) -> if flg /= Html
      then doGrp
      else do
        pTitle
        putStrLn ""
        res'@(Just (T.Group res)) <- doGrp
        let ch = mapMaybe T.tkGroup res :: [[T.Grouped [Named Int64]]]
            results = zip getNOtherGroups $ map (mapMaybe T.tkSimple) ch :: [Named [[Named Int64]]]
            results' = map (fmap (makeAverage . map (map (fmap (fromRational . toRational)))) ) results :: [Named [Named Double]]
        printHtml results' ((commas :: Integer -> String) . round)
        return res'
    (Grouped _ (Singleton{}:_)) -> Just . T.Group <$> mapM (printSimples (lev+1) flg semiSimples . snd) (nubBy (liftExtract2 eqW) semiSimples)
    Grouped{} -> doGrp
    Singleton{} -> error "A single singleton of a WeighResult, this should not happen"
    where
      pTitle = putStrLn $ unwords [replicate lev '#',bname]
      bname = showGrouped act
      doGrp = case nubOtherGroups of
                [] -> do
                  when (flg /= Html) $ putStrLn "\nNo data\n"
                  return Nothing
                real -> Just . T.Group . catMaybes <$> mapM (printReport (lev+1) flg otherGroups . snd) real
      here e = filter (eqG e . snd) arr
      nubOtherGroups = nubBy (liftExtract2 eqG) otherGroups
      getNOtherGroups = map (showGrouped . snd) nubOtherGroups
      otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here act
      semiSimples = mapMaybe (traverse T.tkSimple) otherGroups

-- | Really print the simples, different than printReport for type reason
printSimples :: Int -> StaOut -> [Named WeighResult] -> WeighResult -> IO (T.Grouped [Named Int64])
printSimples lev flg arr act = do
  when (flg == Ascii) $ do
    unless (null bname) $ putStrLn $ unwords [replicate lev '#',bname]
    putStrLn $ TAA.render id id id table
  return $ T.Simple $ map (fmap $ weightAllocatedBytes . fst) filtered
  where
    bname = takeLastAfterBk $ weightLabel $ fst act
    -- filter by the 'act' argument, and sort
    filtered = sortBy (liftExtract2 $ \(x,_) (y,_) -> weightAllocatedBytes x `compare` weightAllocatedBytes y) $ filter (eqW act . snd) arr
    table = TA.Table
      (TA.Group TA.NoLine $ map (TA.Header . fst) filtered)
      (TA.Group TA.SingleLine [TA.Header "AllocatedBytes", TA.Header "GCs"])
      (map ((\(x,y) -> maybe (showWeight x) (\y'->["Errored: "++y']) y) . snd) filtered)

-- | Convert a @Weight@ to a list of @String@ for tabular representation
showWeight :: Weight -> [String]
showWeight w = [commas (weightAllocatedBytes w),show (weightGCs w)]

-- | Name from grouped, necessary for the first level of Grouped for Weigh
groupedToNamed :: Grouped a -> Maybe (Named [Grouped a])
groupedToNamed (Grouped n rst) = Just (n,rst)
groupedToNamed _ = Nothing

-- | Get the childs of a BenchGroup, inserting the name of the library
tkChilds :: Grouped WeighResult -> Maybe [Grouped WeighResult]
tkChilds = groupedToNamed >=> Just . snd

main :: IO ()
main = execParser runSpace >>= main'

main' :: CommandSpace -> IO ()
main' (ListS opt) = case opt of
                    Benchs -> putStr $ unlines $ nub $ map fst Alga.Graph.functions ++ map fst Containers.Graph.functions ++ map fst Fgl.PatriciaTree.functions ++ map fst HashGraph.Gr.functions ++ map fst weighCreationList
                    Libs -> putStr $ unlines $ map fst $ namedWeigh Nothing
main' (RunS only flg libs) = mainWeigh benchs (useResults flg)
  where
    benchs = mapM_ (uncurry wgroup) $ maybe id (\libs' -> filter (flip elem libs' . fst)) libs $ namedWeigh only

namedWeigh :: Maybe [String] -> [Named (Weigh ())]
namedWeigh only =
  [ ("Alga (Algebra.Graph)" , allWeighs (select Alga.Graph.functions) >> weighCreation only Alga.Graph.mk)
  , ("Containers (Data.Graph)" , allWeighs (select Containers.Graph.functions) >> weighCreation only Containers.Graph.mk)
  , ("Fgl (Data.Graph.Inductive.PatriciaTree)" , allWeighs (select Fgl.PatriciaTree.functions) >> weighCreation only Fgl.PatriciaTree.mk)
  , ("Hash-Graph (Data.HashGraph.Strict)" , allWeighs (select HashGraph.Gr.functions) >> weighCreation only HashGraph.Gr.mk)
  ]
  where
    select funcs = maybe funcs (\ols -> filter (\x -> fst x `elem` ols) funcs) only
