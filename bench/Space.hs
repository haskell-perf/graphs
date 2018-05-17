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

groupedToNamed :: Grouped a -> Maybe (Named [Grouped a])
groupedToNamed (Grouped n rst) = Just $ Named n rst
groupedToNamed _ = Nothing

genReport :: Int -- ^ The number of # to write
          -> [Named (Grouped (Weight, Maybe String))] -- ^ The list of benchs
          -> Grouped (Weight, Maybe String) -- ^ A selected bench name
          -> IO ()
genReport lev arr act = do
  let bname = showGrouped act
  unless (null bname) $ putStrLn $ replicate lev '#' ++ " " ++ bname
  case act of
    (Grouped _ (Singleton{}:_)) -> mapM_ (printSimples (lev+1) semiSimples . extract) $ nubBy (liftExtract2 eqW) semiSimples
    Grouped{} -> case nubBy (liftExtract2 eqG) otherGroups of
                   [] -> putStrLn "No data\n"
                   real -> mapM_ (genReport (lev+1) otherGroups . extract) real
    where
      semiSimples = mapMaybe (traverse tkSingl) otherGroups
      otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here act
      here e = filter (eqG e . extract) arr

printSimples :: Int -> [Named (Weight, Maybe String)] -> (Weight, Maybe String) -> IO ()
printSimples lev arr act = do
  let bname = takeLastAfterBk $ weightLabel $ fst act
  unless (null bname) $ putStrLn $ replicate lev '#' ++ " " ++ bname
  putStrLn $ TAA.render id id id table
  where
    filtered = sortBy (liftExtract2 $ \(x,_) (y,_) -> weightAllocatedBytes x `compare` weightAllocatedBytes y) $ filter (liftExtract (eqW act)) arr
    filtered' = map extract filtered
    libs = map show filtered
    table = T.Table
      (T.Group T.NoLine $ map T.Header libs)
      (T.Group T.SingleLine [T.Header "AllocatedBytes", T.Header "GCs"])
      (map (showWeight . fst) filtered')

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

useResults :: [Grouped (Weight, Maybe String)] -> IO ()
useResults res = mapM_ (genReport 2 namedBenchs . extract) benchs'
  where
    namedBenchs = concatMap sequence $ mapMaybe groupedToNamed res
    benchs' = nubBy (liftExtract2 eqG) namedBenchs

main :: IO ()
main = mainWeigh benchs useResults
  where
    benchs = do
      wgroup "Alga (Algebra.Graph)" $ allWeighs Alga.Graph.functions
      wgroup "Containers (Data.Graph)" $ allWeighs Containers.Graph.functions
      wgroup "Fgl (Data.Graph.Inductive.PatriciaTree)" $ allWeighs Fgl.PatriciaTree.functions
      wgroup "Hash-Graph (Data.HashGraph.Strict)" $ allWeighs HashGraph.Gr.functions
