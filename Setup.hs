import Distribution.Simple
import System.Directory
import Data.List (intercalate, delete, isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  generateRealLifeGraphs
  defaultMain


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
