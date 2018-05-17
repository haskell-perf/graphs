import Data.List (nubBy, sortBy, elemIndices)
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad (unless)

import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified HashGraph.Gr

import BenchGraph (allWeighs)
import BenchGraph.Named
import BenchGraph.Utils (mainWeigh)

import Control.Comonad (extract)

import Weigh

import qualified Text.Tabular as T
import qualified Text.Tabular.AsciiArt as TAA

import Options.Applicative (execParser)

import Command

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

-- | Name from grouped, necessary for the first level of Grouped for Weigh
groupedToNamed :: Grouped a -> Maybe (Named [Grouped a])
groupedToNamed (Grouped n rst) = Just $ Named n rst
groupedToNamed _ = Nothing

-- | Print a report from the lists of benchmarks
printReport :: Int -- ^ The number of # to write
            -> Maybe Flag -- ^ Maybe a flag
            -> [Named (Grouped (Weight, Maybe String))] -- ^ The list of benchs
            -> Grouped (Weight, Maybe String) -- ^ A selected bench name
            -> IO ()
printReport lev flg arr act = do
  let bname = showGrouped act
  unless (null bname) $ putStrLn $ replicate lev '#' ++ " " ++ bname
  case act of
    (Grouped _ (Singleton{}:_)) -> mapM_ (printSimples (lev+1) flg semiSimples . extract) $ nubBy (liftExtract2 eqW) semiSimples
    Grouped{} -> case nubBy (liftExtract2 eqG) otherGroups of
                   [] -> putStrLn "No data\n"
                   real -> mapM_ (printReport (lev+1) flg otherGroups . extract) real
    where
      semiSimples = mapMaybe (traverse tkSingl) otherGroups
      otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here act
      here e = filter (eqG e . extract) arr

-- | Really print the simples, different than printReport for type reason
printSimples :: Int -> Maybe Flag -> [Named (Weight, Maybe String)] -> (Weight, Maybe String) -> IO ()
printSimples lev flg arr act = do
  let bname = takeLastAfterBk $ weightLabel $ fst act
  unless (null bname) $ putStrLn $ replicate lev '#' ++ " " ++ bname
  putStrLn $ TAA.render id id id table
  where
    -- filter by the 'act' argument
    filtered = sortBy (liftExtract2 $ \(x,_) (y,_) -> weightAllocatedBytes x `compare` weightAllocatedBytes y) $ filter (liftExtract (eqW act)) arr
    filtered' = map extract filtered
    libs = map show filtered
    table = T.Table
      (T.Group T.NoLine $ map T.Header libs)
      (T.Group T.SingleLine [T.Header "AllocatedBytes", T.Header "GCs"])
      (map (showWeight . fst) filtered')

-- | Convert a @Weight@ to a list of @String@ for tabular representation
showWeight :: Weight -> [String]
showWeight w = [show (weightAllocatedBytes w),show (weightGCs w)]

-- | Take singletons
tkSingl :: Grouped (Weight, Maybe String) -> Maybe (Weight, Maybe String)
tkSingl (Singleton b) = Just b
tkSingl _ = Nothing

-- | Get the childs of a BenchGroup, inserting the name of the library
tkChilds :: Grouped (Weight, Maybe String) -> Maybe [Grouped (Weight, Maybe String)]
tkChilds (Grouped _ childs) = Just childs
tkChilds _ = Nothing

useResults :: Maybe Flag -> [Grouped (Weight, Maybe String)] -> IO ()
useResults flg res = mapM_ (printReport 2 flg namedBenchs . extract) benchs'
  where
    namedBenchs = concatMap sequence $ mapMaybe groupedToNamed res
    benchs' = nubBy (liftExtract2 eqG) namedBenchs

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
