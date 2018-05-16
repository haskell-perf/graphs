import Data.List (nubBy)
import Data.Maybe (mapMaybe)
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

showGrouped :: Grouped a -> String
showGrouped (Grouped n _) = n
showGrouped (Singleton n _) = n

eqG :: Grouped a -> Grouped a -> Bool
eqG a b = showGrouped a == showGrouped b

eqW :: (Weight, a) -> (Weight, a) -> Bool
eqW (x,_) (y,_) = weightLabel x == weightLabel y

groupedToNamed :: Grouped a -> Maybe (Named [Grouped a])
groupedToNamed (Grouped n rst) = Just $ Named n rst
groupedToNamed _ = Nothing

genReport :: Int -- ^ The number of # to write
          -> [Named (Grouped (Weight, Maybe String))] -- ^ The list of benchs
          -> Grouped (Weight, Maybe String) -- ^ A selected bench name
          -> IO ()
genReport _ [] _ = putStrLn "\nNo data\n"
genReport lev arr act = do
  let bname = showGrouped act
  unless (null bname) $ putStrLn $ replicate lev '#' ++ " " ++ bname
  case act of
    (Grouped _ (Singleton{}:_)) -> mapM_ (printSimples (lev+1) semiSimples . extract) $ nubBy (liftExtract2 eqW) semiSimples
    Grouped{} -> mapM_ (genReport (lev+1) otherGroups . extract) $ nubBy (liftExtract2 eqG) otherGroups
    where
      semiSimples = mapMaybe (traverse tkSingl) otherGroups
      otherGroups = concatMap sequence $ mapMaybe (traverse tkChilds) $ here act
      here e = filter (eqG e . extract) arr

printSimples :: Int -> [Named (Weight, Maybe String)] -> (Weight, Maybe String) -> IO ()
printSimples lev arr act = do
  let bname = weightLabel $ fst act
  unless (null bname) $ putStrLn $ replicate lev '#' ++ " " ++ bname
  putStrLn $ unlines $ map showNamed filtered
  where
    filtered = filter (liftExtract (eqW act)) arr
    showNamed (Named k v) = unwords
      [ "* "
      , k
      , ":"
      ,  showWeight (fst v) -- Use the Maybe String ?
      ]

showWeight :: Weight -> String
showWeight w = unwords
  [ "AllocatedBytes:"
  , show (weightAllocatedBytes w)
  , ", GCs :"
  , show (weightGCs w)
  ]

-- | Take singletons
tkSingl :: Grouped (Weight, Maybe String) -> Maybe (Weight, Maybe String)
tkSingl (Singleton _ b) = Just b
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
