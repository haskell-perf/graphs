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
import ListS (listOfSuites, descs)

import BenchGraph.Types
import BenchGraph.Space
import BenchGraph.Named
import BenchGraph.Utils (mainWeigh, defaultGr)

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
useResults (Output su st) todo = do
  putStrLn "Note: results are in bytes"
  mapM_ mapped $ nubBy (liftExtract2 eqG) namedBenchs
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
                printAbstract "lighter" $ T.setBGroup True res''

-- | Print a report from the lists of benchmarks
printReport :: Int -- ^ The number of # to write, must start with 2
            -> StaOut -- ^ Output infos
            -> [Named (Grouped WeighResult)] -- ^ The list of benchs
            -> Grouped WeighResult -- ^ A selected bench name
            -> IO (Maybe (T.Grouped [Named Int64])) -- Maybe if there was actual data
printReport lev flg arr act = case lev of
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
         res'@(Just (T.Group res)) <- doGrp
         let ch = mapMaybe T.tkGroup res :: [[T.Grouped [Named Int64]]]
             results = reverse $ zip getNOtherGroups $ reverse $ map (mapMaybe T.tkSimple) ch :: [Named [[Named Int64]]] -- Double reverse is necessary, since it can lack some data in the front of ch
             results' = map (fmap (makeAverage . map (map (fmap (fromRational . toRational)))) ) results :: [Named [Named Double]]
         printHtml results' ((commas :: Integer -> String) . round)
         return res'
  _ -> do
    when (not (null bname) && flg == Ascii) pTitle
    case act of
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
    getNOtherGroups = reverse $ map (showGrouped . snd) nubOtherGroups
    otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here act
    semiSimples = mapMaybe (traverse T.tkSimple) otherGroups

-- | Really print the simples, different than printReport for type reason
printSimples :: Int -> StaOut -> [Named WeighResult] -> WeighResult -> IO (T.Grouped [Named Int64])
printSimples lev flg arr act = do
  when (flg == Ascii) $ do
    unless (null bname) $ putStrLn $ unwords [replicate lev '#',bname]
    putStrLn $ TAA.render id id id table
  return $ T.Simple (null bname) $ map (fmap $ weightAllocatedBytes . fst) filtered -- False by default, changed after
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
                    Benchs -> putStr $ unlines $ benchsNames Nothing
                    Libs -> putStr $ unlines $ nub $ map fst listOfSuites
main' (RunS only flg libs) = do
  printHeader defaultGr bN
  mainWeigh benchs (useResults flg)
  where
    bN = benchsNames only
    addCrea = if "creation" `elem` bN then (++ listOfCreation) else id
    benchs = mapM_ (uncurry wgroup) $ maybe id (\lbs -> filter (\(n,_) -> n `elem` lbs)) libs $ addCrea $ map (fmap (\(Shadow s) -> allWeigh s)) $ filter filterLN listOfSuites
    filterLN (_,Shadow s) = name s `elem` bN

benchsNames :: Maybe [String] -> [String]
benchsNames only = nub (map (\(_,Shadow s) -> name s)  (maybe id (\e -> filter (\(_,Shadow s) -> name s `elem` e)) only listOfSuites)) ++ listOfCreation'
  where
    listOfCreation' = case only of
                        Nothing -> ["creation"]
                        Just e -> [ "creation" | "creation" `elem` e]

listOfCreation :: [Named (Weigh ())]
listOfCreation  =
  [ ("Alga" , weighCreation Alga.Graph.mk)
  , ("Containers" , weighCreation Containers.Graph.mk)
  , ("Fgl" , weighCreation Fgl.PatriciaTree.mk)
  , ("Hash-Graph" , weighCreation HashGraph.Gr.mk)
  ]

