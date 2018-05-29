import BenchGraph (computeSize)
import BenchGraph.Named

import Data.List (nub, sortBy)

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
  mapM_ printN $ sortBy compare1 here
  where
    here = map fix $ filter (eqDeepSelected selected) arr

printN :: Named Word -> IO ()
printN (n,a) = putStrLn $ unwords [" *", n, ":",show a]

eqDeepSelected :: String -> Named (Named a) -> Bool
eqDeepSelected s = (==) s . shExtr

shExtr :: Named (Named a) -> String
shExtr = fst . snd

main :: IO ()
main = execParser runDataSize >>= main'

main' :: CommandDataSize -> IO ()
main' (RunD gr) = do
  res <- mapM sequence
    [ ("Alga (Algebra.Graph)", computeSize gr Alga.Graph.mk)
    , ("Containers (Data.Graph)", computeSize gr Containers.Graph.mk)
    , ("Fgl (Data.Graph.Inductive.PatriciaTree)", computeSize gr Fgl.PatriciaTree.mk)
    , ("Fgl (Data.Graph.Inductive.Tree)", computeSize gr Fgl.Tree.mk)
    , ("Hash-Graph (Data.HashGraph.Strict)", computeSize gr HashGraph.Gr.mk)
    ]
  let res' = concatMap sequence res
  mapM_ (printNArr res') $ nub $ map shExtr res'
