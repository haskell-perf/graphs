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


printBest :: Ord a => Grouped [Named a] -> IO ()
printBest = printMap . getBest

printMap :: [Named Int] -> IO ()
printMap m = do
  putStrLn "\nSUMMARY:"
  void $ foldMap (\(Named k v) -> putStrLn $ k ++ " was the fastest " ++ show v ++ " times") m
  putStrLn ""

-- | get fastests libraries, sorted
getBest :: Ord a => Grouped [Named a] -> [Named Int]
getBest = sortBy (flip compare) . map toNamed . toList . getBest' empty

getBest' :: Ord a => Map String Int -> Grouped [Named a] -> Map String Int
getBest' m (Simple a) = alter (Just . maybe 1 (+ 1)) (show $ minimum a) m
getBest' m (Group grp) = foldr (unionWith (+) . getBest' m) empty grp
