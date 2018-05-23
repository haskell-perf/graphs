module Abstract
  (
    printAbstract
  )

where

import Data.Map.Strict (Map, unionWith, empty, toList, fromList)
import qualified Data.Map.Strict as M
import Data.List (find, sort)
import Text.Printf (printf)

import BenchGraph.Named

import Types

printAbstract :: String -- ^ A comparative (like "faster")
              -> Grouped [Named Double] -- ^ The actual data
              -> IO ()
printAbstract superlative = printMap superlative . fmap reverse . rearrange . getComparison

-- | Sort the results, plus put the worst lib for comparison
rearrange :: Named [Named Double] -> Named [Named Double]
rearrange (Named n arr) =
  if db >= 1
     then Named n sorted
     else rearrange $ Named n' $ Named n (recip db) : map (fmap (recip db *)) (tail sorted)
  where
    sorted = sort arr
    (Named n' db) = head sorted

printMap :: String -> Named [Named Double] -> IO ()
printMap superlative (Named ref res) = do
  putStrLn "\nABSTRACT:\n"
  mapM_ (\(Named name av) -> putStrLn $ unwords [" *",name,"was",printf "%.2f" av,"times",superlative,"than",ref]) res
  putStrLn ""

average :: Fractional a => [a] -> a
average lst = sum lst / fromRational (toRational (length lst))

-- | The first Name given is the reference name for comparison, the others are the comparison themselves
getComparison :: Grouped [Named Double] -> Named [Named Double]
getComparison grp = Named (ref grp) $ map toNamed $ toList $ M.map average $ getComparison' (ref grp) start grp
  where
    ref = show . head . getRef
    getRef grp' = case grp' of
                   (Simple a) -> a
                   (Group (x:_)) -> getRef x
    start = fromList $ map (\x -> (show x, [])) $ tail $ getRef grp

getComparison' :: String -> Map String [Double] -> Grouped [Named Double] -> Map String [Double]
getComparison' ref m (Simple arr) =
  M.mapMaybeWithKey (\k v -> do
    fs <- get' k arr
    sn <- get' ref arr
    return $ liftExtract2 (/) sn fs : v
    ) m
  where
    get' k = find ((==) k . show)
getComparison' ref m (Group grp) = foldr (unionWith (++) . getComparison' ref m) empty grp

