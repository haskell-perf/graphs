module Best
  (
    printBest
  )
where

import Types

import BenchGraph.Named

import Data.List (sortBy)
import Control.Monad (void)
import Data.Map.Strict (Map, alter, unionWith, empty, toList)

printBest :: Ord a
          => String -- ^ An infix for the output (like "was the fastest")
          -> Grouped [Named a] -- ^ The datas
          -> IO ()
printBest str = printMap str . getBest

printMap :: String -> [Named Int] -> IO ()
printMap str m = do
  putStrLn "\nSUMMARY:\n"
  void $ foldMap (\(Named k v) -> putStrLn $ unwords [" *",k,str,show v,"times"]) m
  putStrLn ""

-- | get fastests libraries, sorted
getBest :: Ord a => Grouped [Named a] -> [Named Int]
getBest = sortBy (flip compare) . map toNamed . toList . getBest' empty

getBest' :: Ord a => Map String Int -> Grouped [Named a] -> Map String Int
getBest' m (Simple a) = alter (Just . maybe 1 (+ 1)) (show $ minimum a) m
getBest' m (Group grp) = foldr (unionWith (+) . getBest' m) empty grp
