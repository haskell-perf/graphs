import BenchGraph (computeSize)
import BenchGraph.Named

import Control.Comonad (extract)
import Data.List (nub, sort)

import Options.Applicative (execParser)

import Command

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
  mapM_ (printNArr' here) $ nub $ map shExtr here
  where
    here = concatMap (sequence . fix) $ filter (eqDeepSelected selected) arr

printNArr' :: [Named (Named Word)] -- ^ Array of all benchs
          -> String -- ^ A selected func
          -> IO ()
printNArr' arr selected = do
  putStrLn $ replicate 3 '#' ++ selected
  mapM_ printN $ sort here
  where
    here = map fix $ filter (eqDeepSelected selected) arr

printN :: Named Word -> IO ()
printN (Named n a) = putStrLn $ unwords [" *", n, ":",show a]

eqDeepSelected :: String -> Named (Named a) -> Bool
eqDeepSelected s = (==) s . shExtr

shExtr :: Named (Named a) -> String
shExtr = show . extract

main :: IO ()
main = execParser runDataSize >>= main'

main' :: CommandDataSize -> IO ()
main' (RunD graphs size) = do
  res <- mapM sequence
    [ Named "Alga (Algebra.Graph)" $ computeSize graphs size Alga.Graph.mk
    , Named "Containers (Data.Graph)" $ computeSize graphs size Containers.Graph.mk
    , Named "Fgl (Data.Graph.Inductive.PatriciaTree)" $ computeSize graphs size Fgl.PatriciaTree.mk
    , Named "Fgl (Data.Graph.Inductive.Tree)" $ computeSize graphs size Fgl.Tree.mk
    , Named "Hash-Graph (Data.HashGraph.Strict)" $ computeSize graphs size HashGraph.Gr.mk
    ]
  let res' = concatMap sequence res
  mapM_ (printNArr res') $ nub $ map shExtr res'
