import BenchGraph (computeSize)
import BenchGraph.Named

import Control.Comonad (extract)
import Data.List (nub, sort)

import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified Fgl.Tree
import qualified HashGraph.Gr

printNArr :: [Named (Named [Named Word])] -- ^ Array of all benchs
          -> String -- ^ A selected func
          -> IO ()
printNArr arr selected = do
  putStrLn $ replicate 2 '#' ++ selected
  mapM_ (printNArr' here) $ nub $ map (show . extract) here
  where
    herePre = filter (\(Named _ (Named f _ )) -> f == selected) arr
    here = concatMap (sequence . fix) herePre

printNArr' :: [Named (Named Word)] -- ^ Array of all benchs
          -> String -- ^ A selected func
          -> IO ()
printNArr' arr selected = do
  putStrLn $ replicate 3 '#' ++ selected
  mapM_ printNArr'' $ sort here
  where
    herePre = filter (\(Named _ (Named f _ )) -> f == selected) arr
    here = map fix herePre

printNArr'' :: Named Word -> IO ()
printNArr'' (Named n a) = putStrLn $ unwords [" *", n, ":",show a]


main :: IO ()
main = do
  res <- mapM sequence
    [ Named "Alga (Algebra.Graph)" $ computeSize size Alga.Graph.mk
    , Named "Containers (Data.Graph)" $ computeSize size Containers.Graph.mk
    , Named "Fgl (Data.Graph.Inductive.PatriciaTree)" $ computeSize size Fgl.PatriciaTree.mk
    , Named "Fgl (Data.Graph.Inductive.Tree)" $ computeSize size Fgl.Tree.mk
    , Named "Hash-Graph (Data.HashGraph.Strict)" $ computeSize size HashGraph.Gr.mk
    ]
  let res' = concatMap sequence res
  mapM_ (printNArr res') $ nub $ map (show . extract) res'
  where
    size = (3,3,2)
