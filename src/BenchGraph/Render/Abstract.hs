module BenchGraph.Render.Abstract
  (
    printAbstract
  , average
  )

where

import Data.Map.Strict (Map, unionWith, empty, toList, fromList)
import qualified Data.Map.Strict as M
import Data.List (find, sortBy)
import Data.Maybe (mapMaybe, fromMaybe)
import Text.Printf (printf)
import Control.Monad (unless)

import BenchGraph.Named

import BenchGraph.Render.Types
import BenchGraph.Render.Common (average)

printAbstract :: String -- ^ A comparative (like "faster")
              -> Grouped [Named Double] -- ^ The actual data
              -> IO ()
printAbstract superlative = printMap superlative . fmap reverse . rearrange . removeNaN . getComparison

removeNaN :: Named [Named Double] -> Named [Named Double]
removeNaN = fmap (mapMaybe (\n -> if isNaN (snd n) then Nothing else Just n))

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
  putStrLn "\nABSTRACT:\n"
  mapM_ (\(name,av) -> putStrLn $ unwords [" *",name,"was",printf "%.2f" av,"times",superlative,"than",ref]) res
  putStrLn ""

-- | The first Name given is the reference name for comparison, the others are the comparison themselves
getComparison :: Grouped [Named Double] -> Named [Named Double]
getComparison grp = case getRef grp of
                      Nothing -> ("",[])
                      Just ref -> let refinedRef = fst ref
                                      start = fromList $ map (\x -> (x, [])) $ snd ref
                                  in (refinedRef, toList $ M.map average $ getComparison' refinedRef start grp)

-- | Return a "reference": A library for wich there is a non-0 time
getRef :: Grouped [Named Double] -> Maybe (String,[String])
getRef (Simple a) = (\x -> (x, filter (x /=) $ map fst a)) . fst <$> find (\(_,x) -> x /= 0) a
getRef (Group (x:xs)) = case getRef x of
                          Nothing -> getRef $ Group xs
                          a -> a
getRef (Group []) = Nothing

getComparison' :: String -> Map String [Double] -> Grouped [Named Double] -> Map String [Double]
getComparison' ref m (Simple arr) =
  M.mapMaybeWithKey (\k v -> do
    fs <- get' k arr
    sn <- get' ref arr
    return $ liftExtract2 (/) sn fs : v
    ) m
  where
    get' k = find ((==) k . fst)
getComparison' ref m (Group grp) = foldr (unionWith (++) . getComparison' ref m) empty grp

