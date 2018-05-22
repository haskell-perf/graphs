{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Best
  (
    printBestI,
    printBest
  )
where

import Types

import BenchGraph.Named

import Data.List (sortBy, sort)
import Control.Monad (void, when)
import Data.Int (Int64)
import Control.Comonad (extract)
import Data.Map.Strict (Map, alter, unionWith, empty, toList)

printBestI :: String -> Grouped [Named Int64] -> IO ()
printBestI s = printBest s . fmap (fmap (fmap (fromRational . toRational)))

printBest :: (Ord a, Fractional a)
          => String -- ^ An infix for the output (like "was the fastest")
          -> Grouped [Named a] -- ^ The datas
          -> IO ()
printBest str = printMap str . getBest

printMap :: String -> (Int,[Named Int]) -> IO ()
printMap str (diff,m) = do
  putStrLn "\nSUMMARY:\n"
  void $ foldMap (\(Named k v) -> putStrLn $ unwords [" *",k,str,show v,"times"]) m
  when (diff /= 0) $ putStrLn $ unwords ["There was",show diff,"ex-aequo"]
  putStrLn ""

-- | get fastests libraries, sorted, retun the number of case where it was not possible to find a "better" one (where the ration between the two best times was inferior to 1.1)
getBest :: (Ord a, Fractional a) => Grouped [Named a] -> (Int,[Named Int])
getBest lst = (lengthG lst - foldr (\x act -> act + extract x) 0 res,res)
  where
    res = sortBy (flip compare) $ map toNamed $ toList $ getBest' empty lst
    lengthG a = case a of
                  Simple{} -> 1
                  Group a' -> sum $ map lengthG a'

getBest' :: (Ord a, Fractional a) => Map String Int -> Grouped [Named a] -> Map String Int
getBest' m (Simple a) = maybe m (\x -> alter (Just . maybe 1 (+ 1)) (show x) m) $ takeVeryBest $ sort a
getBest' m (Group grp) = foldr (unionWith (+) . getBest' m) empty grp

takeVeryBest :: (Ord a, Fractional a) => [Named a] -> Maybe (Named a)
takeVeryBest (x1:x2:_) = if (x1 == x2) || (extract x2 / extract x1) <= 1.1
                            then Nothing
                            else Just x1
takeVeryBest h         = Just $ head h
