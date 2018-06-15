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

  modified <- modif <$> getArgs

  mapM_ (change modified) ["bench/ListS.hs","bench/Time.hs", "bench/Space.hs"]

  defaultMain
  where
    modif args = [T.pack "Alga.Graph" | "--flags=-alga" `elem` args] ++ [T.pack "Fgl.PatriciaTree" | "--flags=-fgl" `elem` args ] ++ [T.pack "HashGraph.Gr" | "--flags=-hashgraph" `elem` args ]

-- | generate real life graphs from a text file
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

-- | Will remove from a file the desired parts
change :: [T.Text] -> String -> IO ()
change modified pref = do
  fil <- T.readFile pref
  let todo = T.unlines $ filter (\x -> not $ any (`T.isInfixOf` x) modified) $ T.lines fil 
  unless (fil == todo) $ T.writeFile pref todo

