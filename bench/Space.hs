import Data.List (nubBy, sortBy, elemIndices)
import Data.Maybe (mapMaybe, catMaybes, isJust, isNothing)
import Data.Int (Int64)
import Control.Monad (unless, when, (>=>))

import Control.Comonad (extract)

import Weigh

import qualified Text.Tabular as T
import qualified Text.Tabular.AsciiArt as TAA

import Options.Applicative (execParser)

import Command
import qualified Types as Ty
import Best

import BenchGraph (allWeighs, weighCreation)
import BenchGraph.Named
import BenchGraph.Utils (mainWeigh)

import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified HashGraph.Gr

showGrouped :: Grouped a -> String
showGrouped (Grouped n _) = n
showGrouped _ = ""

eqG :: Grouped a -> Grouped a -> Bool
eqG a b = showGrouped a == showGrouped b

eqW :: (Weight, a) -> (Weight, a) -> Bool
eqW (x,_) (y,_) = takeLastAfterBk (weightLabel x) == takeLastAfterBk (weightLabel y)

takeLastAfterBk :: String -> String
takeLastAfterBk w = case elemIndices '/' w of
                          [] -> w
                          x -> drop (1+last x) w

useResults :: Maybe Flag -> [Grouped (Weight, Maybe String)] -> IO ()
useResults flg res = mapM_ ((printReport 2 flg namedBenchs . extract) >=> maybe (return ()) (printBest "used the least amount of memory")) benchs'
  where
    namedBenchs = concatMap sequence $ mapMaybe groupedToNamed res
    benchs' = nubBy (liftExtract2 eqG) namedBenchs

-- | Print a report from the lists of benchmarks
printReport :: Int -- ^ The number of # to write
            -> Maybe Flag -- ^ Maybe a flag, it will desactivate all output except the sumarry
            -> [Named (Grouped (Weight, Maybe String))] -- ^ The list of benchs
            -> Grouped (Weight, Maybe String) -- ^ A selected bench name
            -> IO (Maybe (Ty.Grouped [Named Int64])) -- Maybe if there was actual data
printReport lev flg arr act = do
  let bname = showGrouped act
  unless (null bname || (isJust flg && lev /= 2))  $ putStrLn $ replicate lev '#' ++ " " ++ bname
  case act of
    (Grouped _ (Singleton{}:_)) -> Just . Ty.Group <$> mapM (printSimples (lev+1) flg semiSimples . extract) (nubBy (liftExtract2 eqW) semiSimples)
    Grouped{} -> case otherGroups of
                   [] -> do
                     when (isNothing flg) $ putStrLn "No data\n"
                     return Nothing
                   real -> Just . Ty.Group . catMaybes <$> mapM (printReport (lev+1) flg otherGroups . extract) (nubBy (liftExtract2 eqG) real)
    where
      here e = filter (eqG e . extract) arr
      otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here act
      semiSimples = mapMaybe (traverse tkSingl) otherGroups

-- | Really print the simples, different than printReport for type reason
printSimples :: Int -> Maybe Flag -> [Named (Weight, Maybe String)] -> (Weight, Maybe String) -> IO (Ty.Grouped [Named Int64])
printSimples lev flg arr act = do
  let bname = takeLastAfterBk $ weightLabel $ fst act
  unless (null bname || (isJust flg && lev /= 2)) $ putStrLn $ replicate lev '#' ++ " " ++ bname
  unless (isJust flg) $ putStrLn $ TAA.render id id id table
  return $ Ty.Simple $ map (fmap $ weightAllocatedBytes . fst) filtered
  where
    -- filter by the 'act' argument, and sort
    filtered = sortBy (liftExtract2 $ \(x,_) (y,_) -> weightAllocatedBytes x `compare` weightAllocatedBytes y) $ filter (liftExtract (eqW act)) arr
    table = T.Table
      (T.Group T.NoLine $ map (T.Header . show) filtered)
      (T.Group T.SingleLine [T.Header "AllocatedBytes", T.Header "GCs"])
      (map (showWeight . fst . extract) filtered)

-- | Convert a @Weight@ to a list of @String@ for tabular representation
showWeight :: Weight -> [String]
showWeight w = [show (weightAllocatedBytes w),show (weightGCs w)]

-- | Take singletons
tkSingl :: Grouped (Weight, Maybe String) -> Maybe (Weight, Maybe String)
tkSingl (Singleton b) = Just b
tkSingl _ = Nothing

-- | Name from grouped, necessary for the first level of Grouped for Weigh
groupedToNamed :: Grouped a -> Maybe (Named [Grouped a])
groupedToNamed (Grouped n rst) = Just $ Named n rst
groupedToNamed _ = Nothing

-- | Get the childs of a BenchGroup, inserting the name of the library
tkChilds :: Grouped (Weight, Maybe String) -> Maybe [Grouped (Weight, Maybe String)]
tkChilds = groupedToNamed >=> Just . extract

main :: IO ()
main = execParser flagSpace >>= main'

main' :: Maybe Flag -> IO ()
main' opts = mainWeigh benchs (useResults opts)
  where
    benchs = do
      wgroup "Alga (Algebra.Graph)" $ allWeighs Alga.Graph.functions
      wgroup "Containers (Data.Graph)" $ allWeighs Containers.Graph.functions
      wgroup "Fgl (Data.Graph.Inductive.PatriciaTree)" $ allWeighs Fgl.PatriciaTree.functions
      wgroup "Hash-Graph (Data.HashGraph.Strict)" $ allWeighs HashGraph.Gr.functions
      wgroup "Alga (Algebra.Graph)" $ weighCreation Alga.Graph.mk
      wgroup "Containers (Data.Graph)" $ weighCreation Containers.Graph.mk
      wgroup "Fgl (Data.Graph.Inductive.PatriciaTree)" $ weighCreation Fgl.PatriciaTree.mk
      wgroup "Hash-Graph (Data.HashGraph.Strict)" $ weighCreation HashGraph.Gr.mk
