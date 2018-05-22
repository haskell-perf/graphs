module Compare
  (
  compareResults
  )

where

import Data.Map.Strict (Map, unionWith, empty, toList, fromList)
import qualified Data.Map.Strict as M
import Data.List (find)

import BenchGraph.Named

import Types

compareResults :: Grouped [Named Double] -> IO ()
compareResults = printMap . getComparison

printMap :: (String,[Named [Double]]) -> IO ()
printMap (ref,res) = mapM_ (\(Named name arr) -> putStrLn $ unwords ["*",name,"was",show $ average arr,"times faster than",ref]) res

average :: Fractional a => [a] -> a
average lst = sum lst / fromRational (toRational (length lst))

getComparison :: Grouped [Named Double] -> (String,[Named [Double]])
getComparison grp = (ref grp, map toNamed $ toList $ getComparison' (ref grp) start grp)
  where
    ref = show . head . getRef
    getRef grp = case grp of
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

