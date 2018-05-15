import Data.List (nub, nubBy)
import Data.Maybe (mapMaybe)

import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified HashGraph.Gr

import BenchGraph (allWeighs)
import BenchGraph.Named
import BenchGraph.Utils (mainWeigh)

import Weigh

showGrouped :: Grouped a -> String
showGrouped (Grouped n _) = n
showGrouped (Singleton n _) = n

eqG :: Grouped a -> Grouped a -> Bool
eqG a b = showGrouped a == showGrouped b

groupedToNamed :: Grouped a -> Maybe (Named [Grouped a])
groupedToNamed (Grouped n rst) = Just $ Named n rst
groupedToNamed _ = Nothing

useResults :: [Grouped (Weight, Maybe String)] -> IO ()
useResults res = putStrLn $ unlines $ map (liftExtract show) namedBenchs
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
