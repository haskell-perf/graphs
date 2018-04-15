import Data.List (sort, sortBy, filter, nub)
import Data.Maybe (mapMaybe)

import Criterion
import Criterion.Types

import Statistics.Types

import qualified Alga
import qualified Containers
import qualified Fgl

data BReport = Simple String String Report | Group String [BReport]

instance Show BReport where
  show (Simple _ name _) = name
  show (Group name _) = name

instance Ord BReport where
  a <= b = show a <= show b 

instance Eq BReport where
  a == b = show a == show b 

runSingleBenchmark :: String -> Benchmark -> IO BReport
runSingleBenchmark str (Benchmark name benchm) = Simple str name <$> benchmark' benchm
runSingleBenchmark str (BenchGroup name benchmarks) = Group name <$> mapM (runSingleBenchmark str) benchmarks

genReport :: [BReport] -> String 
genReport todo = "# Compare benchmarks \n" ++ genReport' 2 todo

genReport' :: Int -> [BReport] -> String
genReport' lev arr = unlines $ map toPrint $ nub arr 
  where
    toPrint breport = replicate lev '#' ++ " " ++ show breport ++ "\n" ++ why breport
    why br = case br of
      Simple{} -> unlines $ map (\(a,b,c) -> "* "++ a ++ " : "++ show (getMean c) ++ " s. (Mean)" )$ sortBy (\(_,_,a) (_,_,b) -> getMean a `compare` getMean b) $ tkSimple $ here br
      Group{} -> genReport' (lev+1) $ concat $ mapMaybe tkList $ here br 
    here e = filter (e==) arr

tkSimple :: [BReport] -> [(String,String,Report)]
tkSimple = mapMaybe (\x -> case x of
                       Simple a b c -> Just (a,b,c)
                       Group _ _ -> Nothing)

tkList :: BReport -> Maybe [BReport]
tkList Simple{} = Nothing
tkList (Group _ b) = Just b

getMean :: Report -> Double
getMean = estPoint . anMean . reportAnalysis

main :: IO ()
main = do
  let todo = [("Alga",Alga.allBenchs), ("Containers",Containers.allBenchs), ("Fgl", Fgl.allBenchs)]
  allBenchs <- mapM (\(x,y) -> mapM (runSingleBenchmark x) y) todo 
  putStrLn $ genReport $ sort $ concat allBenchs 
  return ()
