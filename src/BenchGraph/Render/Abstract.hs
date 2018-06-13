module BenchGraph.Render.Abstract
  (
    printAbstract
  , average
  )

where

import Data.List (find, sortBy)
import Text.Printf (printf)
import Control.Monad (unless)
import Data.Bitraversable (bisequence)

import BenchGraph.Named

import BenchGraph.Render.Types
import BenchGraph.Render.Common (average)

-- | Will print an abstract, comparing libraries by their average time
printAbstract :: String -- ^ A comparative (like "faster")
              -> Grouped [Named Double] -- ^ The actual data
              -> IO ()
printAbstract comparative = printMap comparative . fmap reverse . rearrange . removeNaN . getComparison . getSimples

removeNaN :: Named [Named Double] -> Named [Named Double]
removeNaN = fmap (filter (not . isNaN . snd))

getSimples :: Grouped [Named Double] -> [[Named Double]]
getSimples (Simple b v) = if b then [v] else [[]]
getSimples (Group lst) = filter (not . null) $ concatMap getSimples lst

-- | Sort the results, plus put the worst lib for comparison
rearrange :: Named [Named Double] -> Named [Named Double]
rearrange na@(n,arr) =
  if not $ null arr
     then if db >= 1
             then (n,sorted)
             else rearrange (n',(n,recip db) : map (fmap (recip db *)) (tail sorted))
    else na
  where
    sorted = sortBy compare1 arr
    (n',db) = head sorted

printMap :: String -> Named [Named Double] -> IO ()
printMap superlative (ref,res) = unless (null res) $ do
  putStrLn $ unlines ["\nABSTRACT:","(Based on an average of the ratio between largest benchmarks)"]
  mapM_ (\(name,av) -> putStrLn $ unwords [" *",name,"was",printf "%.2f" av,"times",superlative,"than",ref]) res
  putStrLn ""

-- | The first Name given is the reference name for comparison, the others are the comparison themselves
getComparison :: [[Named Double]] -> Named [Named Double]
getComparison grp = case getRef grp of
                      Nothing -> ("",[])
                      Just ref -> let refinedRef = fst ref
                                      start = snd ref
                                  in (refinedRef, map (\x -> (x,getComparison' grp refinedRef x)) start)

-- | Return a "reference": A library for wich there is a non-0 value
getRef :: [[Named Double]] -> Maybe (String,[String])
getRef [] = Nothing
getRef (x:xs) = case td of
                  Nothing -> getRef xs
                  a -> a
  where
    td = do
      val <- fst <$> find (\(_,y) -> y /= 0) x
      return (val, map fst $ filter (\(n,_) -> n /= val) x)

getComparison' :: [[Named Double]] -> String -> String -> Double
getComparison' grp ref todo = average $ map (\lst ->
  maybe 0 (uncurry (/)) $ bisequence (lookup ref lst,lookup todo lst))
  grp

