import Distribution.Simple
import System.Directory
import Data.List (intercalate, delete, isInfixOf)
import Control.Monad (unless, when)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  args <- getArgs
  exis <- doesFileExist "src/BenchGraph/RealLife/Generated.hs"
  let doWeCreateGenerated = not $ hasNotFlag "reallife" args

  unless exis $ generateRealLifeGraphs doWeCreateGenerated

  mapM_ (change (modif args)) ["bench/ListS.hs","bench/Time.hs", "bench/Space.hs"]

  defaultMain
  where
    modif args = [T.pack "Alga.Graph" | hasNotFlag "alga" args] ++ [T.pack "Fgl.PatriciaTree" | hasNotFlag "fgl" args ] ++ [T.pack "HashGraph.Gr" | hasNotFlag "hashgraph" args ]
    hasNotFlag str = any (isInfixOf ('-':str))

-- | generate real life graphs from a text file
generateRealLifeGraphs :: Bool -> IO ()
generateRealLifeGraphs b = if b
  then do
    gr <- delete "README.md" <$> listDirectory prefixDir
    str <- foldl accumF (return start) gr
    writeFile generatedFile $ init (init str) ++ "\n  ]"
  else writeFile generatedFile $ start ++ "  ]"
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

