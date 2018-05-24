import Data.List (nub, nubBy, sortBy, elemIndices)
import Data.Function (on)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Int (Int64)
import Control.Monad (when, unless, (>=>))

import Control.Comonad (extract)

import Weigh (Grouped (..), Weight (..), Weigh, wgroup, commas)

import qualified Text.Tabular as T
import qualified Text.Tabular.AsciiArt as TAA

import Options.Applicative (execParser)

import Command
import qualified Types as Ty
import Best
import Abstract

import BenchGraph
import BenchGraph.Named
import BenchGraph.Utils (mainWeigh)

import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified Fgl.Tree
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
useResults flg todo = mapM_ mapped $ nubBy (liftExtract2 eqG) namedBenchs
  where
    namedBenchs = concatMap sequence $ mapMaybe groupedToNamed todo
    mapped e = do
      res <- printReport 2 flg namedBenchs $ extract e
      case res of
        Nothing -> return ()
        Just res' ->
          let res'' = fmap (fmap (fmap (fromRational . toRational))) res'
              in when (sumOut flg) $ do
                printBest "used the least amount of memory" res''
                printAbstract "lighter" res''

-- | Print a report from the lists of benchmarks
printReport :: Int -- ^ The number of # to write
            -> Output -- ^ Output infos
            -> [Named (Grouped WeighResult)] -- ^ The list of benchs
            -> Grouped WeighResult -- ^ A selected bench name
            -> IO (Maybe (Ty.Grouped [Named Int64])) -- Maybe if there was actual data
printReport lev flg arr act = do
  when (not (null bname) && (staOut flg || lev == 2)) $ putStrLn $ unwords [replicate lev '#',bname]
  case act of
    (Grouped _ (Singleton{}:_)) -> Just . Ty.Group <$> mapM (printSimples (lev+1) flg semiSimples . extract) (nubBy (liftExtract2 eqW) semiSimples)
    Grouped{} -> case otherGroups of
                   [] -> do
                     when (staOut flg) $ putStrLn "No data\n"
                     return Nothing
                   real -> Just . Ty.Group . catMaybes <$> mapM (printReport (lev+1) flg otherGroups . extract) (nubBy (liftExtract2 eqG) real)
    Singleton{} -> error "A single singleton of a WeighResult, this should not happen"
    where
      bname = showGrouped act
      here e = filter (eqG e . extract) arr
      otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here act
      semiSimples = mapMaybe (traverse tkSingl) otherGroups

-- | Really print the simples, different than printReport for type reason
printSimples :: Int -> Output -> [Named WeighResult] -> WeighResult -> IO (Ty.Grouped [Named Int64])
printSimples lev flg arr act = do
  when (staOut flg) $ do
    unless (null bname) $ putStrLn $ unwords [replicate lev '#',bname]
    putStrLn $ TAA.render id id id table
  return $ Ty.Simple $ map (fmap $ weightAllocatedBytes . fst) filtered
  where
    bname = takeLastAfterBk $ weightLabel $ fst act
    -- filter by the 'act' argument, and sort
    filtered = sortBy (liftExtract2 $ \(x,_) (y,_) -> weightAllocatedBytes x `compare` weightAllocatedBytes y) $ filter (liftExtract (eqW act)) arr
    table = T.Table
      (T.Group T.NoLine $ map (T.Header . show) filtered)
      (T.Group T.SingleLine [T.Header "AllocatedBytes", T.Header "GCs"])
      (map ((\(x,y) -> maybe (showWeight x) (\y'->["Errored: "++y']) y) . extract) filtered)

-- | Convert a @Weight@ to a list of @String@ for tabular representation
showWeight :: Weight -> [String]
showWeight w = [commas (weightAllocatedBytes w),show (weightGCs w)]

-- | Take singletons
tkSingl :: Grouped WeighResult -> Maybe WeighResult
tkSingl (Singleton b) = Just b
tkSingl _ = Nothing

-- | Name from grouped, necessary for the first level of Grouped for Weigh
groupedToNamed :: Grouped a -> Maybe (Named [Grouped a])
groupedToNamed (Grouped n rst) = Just $ Named n rst
groupedToNamed _ = Nothing

-- | Get the childs of a BenchGroup, inserting the name of the library
tkChilds :: Grouped WeighResult -> Maybe [Grouped WeighResult]
tkChilds = groupedToNamed >=> Just . extract

main :: IO ()
main = execParser runSpace >>= main'

main' :: CommandSpace -> IO ()
main' (ListS opt) = case opt of
                    Benchs -> putStr $ unlines $ nub $ map show Alga.Graph.functions ++ map show Containers.Graph.functions ++ map show Fgl.PatriciaTree.functions ++ map show HashGraph.Gr.functions ++ map show Fgl.Tree.functions ++ map show weighCreationList
                    Libs -> putStr $ unlines $ map show $ namedWeigh Nothing
main' (RunS only flg libs) = mainWeigh benchs (useResults flg)
  where
    benchs = mapM_ (uncurry wgroup . fromNamed) $ maybe id (\libs' -> filter (flip elem libs' . show)) libs $ namedWeigh only

namedWeigh :: Maybe String -> [Named (Weigh ())]
namedWeigh only =
  [ Named "Alga (Algebra.Graph)" $ allWeighs (select Alga.Graph.functions) >> weighCreation only Alga.Graph.mk
  , Named "Containers (Data.Graph)" $ allWeighs (select Containers.Graph.functions) >> weighCreation only Containers.Graph.mk
  , Named "Fgl (Data.Graph.Inductive.PatriciaTree)" $ allWeighs (select Fgl.PatriciaTree.functions) >> weighCreation only Fgl.PatriciaTree.mk
  , Named "Fgl (Data.Graph.Inductive.Tree)" $ allWeighs (select Fgl.Tree.functions) >> weighCreation only Fgl.Tree.mk
  , Named "Hash-Graph (Data.HashGraph.Strict)" $ allWeighs (select HashGraph.Gr.functions) >> weighCreation only HashGraph.Gr.mk
  ]
  where
    select funcs = maybe funcs (\x -> filter ((==) x . show) funcs) only
