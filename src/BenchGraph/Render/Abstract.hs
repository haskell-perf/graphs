module BenchGraph.Render.Abstract
  (
    printAbstract
  , average
  )

where

import Data.List (find, sortBy)
import Data.Maybe (mapMaybe, fromMaybe)
import Text.Printf (printf)
import Control.Monad (unless)

import BenchGraph.Named

import BenchGraph.Render.Types
import BenchGraph.Render.Common (average)

-- | Will print an abstract, comparing libraries by their average time
printAbstract :: String -- ^ A comparative (like "faster")
              -> Grouped [Named Double] -- ^ The actual data
              -> IO ()
printAbstract comparative = printMap comparative . rearrange . removeNaN . getComparison

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
  putStrLn $ unlines ["\nABSTRACT:","(Calculated using simple linear regression)"]
  mapM_ (\(name,av) -> putStrLn $ unwords [" *",name,"was",printf "%.2f" av,"times",superlative,"than",ref]) res
  putStrLn ""

-- | The first Name given is the reference name for comparison, the others are the comparison themselves
getComparison :: Grouped [Named Double] -> Named [Named Double]
getComparison grp = case getRef grp of
                      Nothing -> ("",[])
                      Just ref -> let refinedRef = fst ref
                                      start = snd ref
                                  in (refinedRef, map (\x -> (x,beta refinedRef grp x)) start)

-- | Return a "reference": A library for wich there is a non-0 time
getRef :: Grouped [Named Double] -> Maybe (String,[String])
getRef (Simple a) = (\x -> (x, filter (x /=) $ map fst a)) . fst <$> find (\(_,x) -> x /= 0) a
getRef (Group (x:xs)) = case getRef x of
                          Nothing -> getRef $ Group xs
                          a -> a
getRef (Group []) = Nothing

-- | Calculate the linear coefficient between times of  a library and times of a reference
beta :: String -> Grouped [Named Double] -> String -> Double
beta ref grp todo = sum (map (\(x,y) -> (x-todoAvg) * (y-refAvg)) zipped) / sum (map (\x -> (x- todoAvg)**2) todoT)
  where
    zipped = zip todoT refT
    refT = getTimesByName ref grp
    refAvg = average refT
    todoT = getTimesByName todo grp
    todoAvg = average todoT

getTimesByName :: String -> Grouped [Named Double] -> [Double]
getTimesByName s (Simple xs) = maybe [] return $ lookup s xs
getTimesByName s (Group grp) = concatMap (getTimesByName s) grp

