module BenchGraph.Render.Common where

import BenchGraph.Named
import Control.Comonad (extract, extend)

import qualified Text.Tabular as T
import qualified Text.Tabular.Html as TH

import Text.Html (stringToHtml)
import Data.List (transpose, intersperse)
import Data.Char (toLower)

average :: Fractional a => [a] -> a
average lst = sum lst / fromRational (toRational (length lst))

makeAverage :: [[Named Double]] -> [Named Double]
makeAverage arr = map (extend (average . mk)) $ head arr
  where
    mk (n,_) = map extract $ concatMap (filter ((==) n . fst)) arr

printHtml :: [Named [Named Double]]
          -> (Double -> String)
          -> IO ()
printHtml arr ren = print $ TH.render stringToHtml stringToHtml stringToHtml table
  where
    libs = map fst $ extract $ head arr
    cases = map fst arr
    content = transpose $ map (map (ren . extract) . extract) arr
    table = T.Table
      (T.Group T.NoLine $ map T.Header libs)
      (T.Group T.SingleLine $ map T.Header cases)
      content

printHeader :: [Named Int] -> [String] -> IO ()
printHeader gr todo = putStrLn $ unlines ["# Compare benchmarks\n","Doing:","\n----",unlines $ map (\x ->"* [" ++ x ++"](#"++ unwords (intersperse "-" $ words $ map toLower x) ++")") todo ++ ["----"],unwords ["Using",show gr,"as graphs"]]

