{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}

import Data.List (nub, nubBy, sort, sortBy, elemIndices)
import Data.Function (on)
import Data.Maybe (mapMaybe, catMaybes, isJust)
import Data.Int (Int64)
import Control.Monad (when, unless, (>=>), forM_)
import System.Environment (lookupEnv)
import Data.Foldable (sequence_)

import Weigh (Grouped (..), Weight (..), Weigh, wgroup, commas, weighResults)

import qualified Text.Tabular as TA
import qualified Text.Tabular.AsciiArt as TAA

import Options.Applicative (execParser)

import Command
import ListS (listOfSuites, descs)

import BenchGraph.Types
import BenchGraph.Space
import BenchGraph.Named
import BenchGraph.Utils (defaultGr)

import qualified BenchGraph.Render.Types as T
import BenchGraph.Render.Best
import BenchGraph.Render.Abstract
import BenchGraph.Render.Common

#ifdef CHART
import BenchGraph.Render.Chart
#endif

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

useResults :: Output -> [Named (Named String)] -> [Grouped (Weight, Maybe String)] -> IO ()
useResults flg notDef todo = do
  putStrLn "Note: results are in bytes"
  results <- fmap catMaybes $ mapM mapped $ nubBy (liftExtract2 eqG) namedBenchs
  maybe (return ()) (\x -> writeFile x $ show results) $ saveToFile flg
  case figOut flg of
    Nothing -> return ()
    (Just x) -> renderG x results
  where
    namedBenchs = concatMap sequence $ mapMaybe groupedToNamed todo
    mapped e = do
      putStrLn $ unwords [replicate 2 '#', showGrouped $ snd e]
      maybe (return ()) (putStrLn . (++) "\nDescription: ") (lookup (showGrouped $ snd e) descs)
      putStrLn ""
      res <- printReport 2 (staOut flg) namedBenchs $ snd e
      forM_ (filter (\(_,(a,_)) -> a == showGrouped (snd e)) notDef) $ \no -> putStrLn $ unwords ["Not implemented for",fst no,"because",snd (snd no)] ++ "."
      case res of
        Nothing -> return Nothing
        Just res' ->
          let res'' = fmap (fmap (fmap (fromRational . toRational))) res'
              in do
                let onlyLargeBenchs = T.removeTailLast res''
                when (sumOut flg) $ do
                  printBest "used the least amount of memory" res''
                  printAbstract "lighter" onlyLargeBenchs
                return $ Just (showGrouped $ snd e, onlyLargeBenchs)

renderG :: T.ChartOutput -> [Named (T.Grouped [Named Double])] -> IO ()
#ifdef CHART
renderG x results = mkChart "Space results" defaultGr show x $ Left $ sortBy (on compare fst) results
#else
renderG _ _ = return ()
#endif

-- | Print a report from the lists of benchmarks
printReport :: Int -- ^ The number of # to write, must start with 2
            -> StaOut -- ^ Output infos
            -> [Named (Grouped WeighResult)] -- ^ The list of benchs
            -> Grouped WeighResult -- ^ A selected bench name
            -> IO (Maybe (T.Grouped [Named Int64])) -- Maybe if there was actual data
printReport lev flg arr act = case lev of
  2 -> doGrp
  3 -> do
    when (flg /= Null) pTitle
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
                when (flg == Ascii) $ putStrLn "\nNo data\n"
                return Nothing
              real -> do
                grp <- catMaybes <$> mapM (printReport (lev+1) flg otherGroups . snd) real
                return $ Just $ T.Group $ (if lev == 3 then (map (T.setGName bname)) else id) grp
    here e = filter (eqG e . snd) arr
    nubOtherGroups = nubBy (liftExtract2 eqG) otherGroups
    getNOtherGroups = reverse $ map (showGrouped . snd) nubOtherGroups
    otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here act
    semiSimples = mapMaybe (traverse tkSimple) otherGroups

-- | Really print the simples, different than printReport for type reason
printSimples :: Int -> StaOut -> [Named WeighResult] -> WeighResult -> IO (T.Grouped [Named Int64])
printSimples lev flg arr act = do
  when (flg == Ascii) $ do
    unless (null bname) $ putStrLn $ unwords [replicate lev '#',bname]
    putStrLn $ TAA.render id id id table
  return $ T.Simple "" $ map (fmap $ weightAllocatedBytes . fst) filtered -- False by default, changed after
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
main = execParser commandP >>= main'

main' :: Command -> IO ()
main' (List opt) = case opt of
                    Benchs -> putStr $ unlines $ benchsNames Nothing Nothing
                    Libs -> putStr $ unlines $ nub $ map fst listOfSuites
main' (Render fp opt) = readFile fp >>= renderG opt . read
main' (Run only notonly flg libs _ _ _) = do
  printHeader defaultGr bN
  mainWeigh benchs (useResults flg (mapMaybe (\(n,Shadow s) -> either (\x -> Just (n,x)) (const Nothing) s ) filteredArr))
  where
    bN = benchsNames only notonly
    benchs = map (uncurry wgroup) $ filterLib $ addCrea $ mapMaybe (\(n,Shadow a) -> either (const Nothing) (\x -> Just (n,allWeigh x)) a) filteredArr
    filterLib = maybe id (\lbs -> filter (\(n,_) -> n `elem` lbs)) libs
    addCrea = if "creation" `elem` bN then (++ listOfCreation) else id
    filteredArr = filter (`isNameIn` bN) listOfSuites

benchsNames :: Maybe Option -> Maybe [String] -> [String]
benchsNames only notonly = nub $ useNotOnly $ useOnly extractedNames
  where
    extractedNames = sort $ "creation" : map (\(_,Shadow s) -> either fst name s) listOfSuites
    useOnly = case only of
      Nothing -> id
      (Just (Only lst)) -> filter (`elem` lst)
      (Just (Part one' two)) -> \as ->
          let one = one' + 1
              per = length as `div` two
              f   = if one' + 1 == two then id else take (one*per)
           in drop ((one-1)*per) $ f as
    useNotOnly = case notonly of
                   Nothing -> id
                   (Just lst) -> filter (`notElem` lst)

isNameIn :: (a,ShadowedS) -> [String] -> Bool
isNameIn (_,Shadow s) e = either fst name s `elem` e

listOfCreation :: [Named (Weigh ())]
listOfCreation  =
  [ ("Containers" , weighCreation Containers.Graph.mk)
#ifdef ALGA
  , ("Alga" , weighCreation Alga.Graph.mk)
#endif
#ifdef FGL
  , ("Fgl" , weighCreation Fgl.PatriciaTree.mk)
#endif
#ifdef HASHGRAPH
  , ("Hash-Graph" , weighCreation HashGraph.Gr.mk)
#endif
  ]

mainWeigh :: [Weigh ()] -> ([Grouped (Weight, Maybe String)] -> IO ()) -> IO ()
mainWeigh rights f = do
  args <- lookupEnv "WEIGH_CASE"
  (results,_) <- weighResults $ sequence_ rights
  unless (isJust args) $ f results

-- | Util for Weigh Grouped
tkSimple :: Grouped a -> Maybe a
tkSimple (Singleton a) = Just a
tkSimple _ = Nothing
