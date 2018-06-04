import BenchGraph (computeSize)
import BenchGraph.Named

import Data.List (nub, sortBy)

import Options.Applicative (execParser)

import Command

import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified HashGraph.Gr

import BenchGraph.Utils (defaultGr)

printNArr :: [Named (Named [Named Word])] -- ^ Array of all benchs
          -> String -- ^ A selected func
          -> IO ()
printNArr arr selected = do
  putStrLn $ replicate 2 '#' ++ " " ++ selected ++ "\n"
  mapM_ (printNArr' here) $ nub $ map shExtr here
  where
    here = concatMap (sequence . fix) $ filter (eqDeepSelected selected) arr

printNArr' :: [Named (Named Word)] -- ^ Array of all benchs
          -> String -- ^ A selected func
          -> IO ()
printNArr' arr selected = do
  putStrLn $ replicate 3 '#' ++ " " ++ selected ++ "\n"
  mapM_ printN $ sortBy compare1 here
  putStrLn ""
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
main' (RunD gr') = do
  res <- mapM sequence
    [ ("Alga", computeSize gr Alga.Graph.mk)
    , ("Containers", computeSize gr Containers.Graph.mk)
    , ("Fgl", computeSize gr Fgl.PatriciaTree.mk)
    , ("Hash-Graph", computeSize gr HashGraph.Gr.mk)
    ]
  let res' = concatMap sequence res
  putStrLn "Note: Results are in bytes\n"
  mapM_ (printNArr res') $ nub $ map shExtr res'
  where
    gr = case gr' of
           [] -> defaultGr
           g -> g
