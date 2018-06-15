import Distribution.Simple
import System.Directory
import Data.List (intercalate, delete, isInfixOf)
import Control.Monad (unless)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  b <- doesFileExist "src/BenchGraph/RealLife/Generated.hs"
  unless b generateRealLifeGraphs

  args <- getArgs

  let timeStr = "bench/Time.hs"
  let listSStr = "bench/ListS.hs"

  change args timeStr 
  change args listSStr 

  defaultMain

generateRealLifeGraphs :: IO ()
generateRealLifeGraphs = do
  gr <- delete "README.md" <$> listDirectory prefixDir
  str <- foldl accumF (return start) gr
  writeFile generatedFile $ init (init str) ++ "\n  ]"
  where
    accumF str' filegr = str' >>= \str -> do
      edges <- readFile $ prefixDir ++ filegr
      let edges' = map ((\[x,y] -> "  ( " ++ x ++ " , "++y++" )") . words) $ drop 2 $ lines edges
      return $ unlines [str,"  [",intercalate "," edges',"  ],"]
    prefix = "src/BenchGraph/RealLife/"
    prefixDir = prefix ++ "Graphs/"
    generatedFile = prefix ++ "Generated.hs"
    start = unlines
      [ "module BenchGraph.RealLife.Generated (generated) where"
      , ""
      , "generated :: [[(Int,Int)]]"
      , "generated = ["
      ]

change :: [String] -> String -> IO ()
change args pref = do
  let rmAlga = if "--flags=-alga" `elem` args then [T.pack "Alga.Graph"] else []
  let rmFgl = if "--flags=-fgl" `elem` args then [T.pack "Fgl.PatriciaTree"] else []
  let rmHashGraph = if "--flags=-hashgraph" `elem` args then [T.pack "HashGraph.Gr"] else []
  
  fil <- T.readFile pref

  T.writeFile pref $ T.unlines $ filter (\x -> not $ any (`T.isInfixOf` x) $ rmAlga ++ rmFgl ++ rmHashGraph) $ T.lines fil 


