{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module BenchGraph.Render.Best
  (
    printBest
  )
where

import BenchGraph.Render.Types

import BenchGraph.Named

import Data.List (sortBy, sort)
import Control.Monad (void, when)
import Control.Comonad (extract)
import Data.Map.Strict (Map, alter, unionWith, empty, toList)

printBest :: String -- ^ An infix for the output (like "was the fastest")
          -> Grouped [Named Double] -- ^ The datas
          -> IO ()
printBest str = printMap str . getBest

printMap :: String -> (Int,[Named Int]) -> IO ()
printMap str (diff,m) = do
  putStrLn "\nSUMMARY:\n"
  void $ foldMap (\(Named k v) -> putStrLn $ unwords [" *",k,str,show v,"times"]) m
  when (diff /= 0) $ putStrLn $ unwords ["There was",show diff,"ex-aequo"]
  putStrLn ""

-- | get fastests libraries, sorted, retun the number of case where it was not possible to find a "better" one (where the ration between the two best times was inferior to 1.1)
getBest :: Grouped [Named Double] -> (Int,[Named Int])
getBest lst = (lengthG lst - foldr (\x act -> act + extract x) 0 res,res)
  where
    res = sortBy (flip compare) $ map toNamed $ toList $ getBest' empty lst

getBest' :: Map String Int -> Grouped [Named Double] -> Map String Int
getBest' m (Simple a) = maybe m (\x -> alter (Just . maybe 1 (+ 1)) (show x) m) $ takeVeryBest $ sort a
getBest' m (Group grp) = foldr (unionWith (+) . getBest' m) empty grp

takeVeryBest :: [Named Double] -> Maybe (Named Double)
takeVeryBest (x1:x2:_) = if (x1 == x2) || (extract x2 / extract x1) <= 1.1
                            then Nothing
                            else Just x1
takeVeryBest h         = Just $ head h
