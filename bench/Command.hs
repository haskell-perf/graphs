{-# LANGUAGE CPP #-}
module Command
  (
  Option (..),
  Output (..),
  Command (..),
  ListOption (..),
  StaOut (..),
  CommandDataSize (..),
  commandP,
  commandDataSize
  )

where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import BenchGraph.Render.Types

import ListS (listOfSuites)

data Option = Part Int Int | Only [String]
  deriving (Show, Eq)

data StaOut = Ascii | Html | Null | QuickComparison deriving (Read, Show, Eq)

staOutCons :: [String]
staOutCons = ["Ascii", "Html", "Null", "QuickComparison"]

data Output = Output {
  sumOut :: Bool, -- ^ Output summary ?
  saveToFile :: Maybe String,
  staOut :: StaOut, -- ^ Output standard ?
  figOut :: Maybe ChartOutput -- ^ Output figures ?
  }
  deriving (Read, Show, Eq)

type Lib = String
type Graph = String

data ListOption = Benchs | Libs
  deriving (Show, Eq)

data Command = List ListOption | Run (Maybe Option) (Maybe [String]) Output (Maybe [Lib]) Bool Bool [(Graph,Int)] | Render String ChartOutput
  deriving (Show, Eq)

newtype CommandDataSize = RunD [(Graph,Int)]

partOpt :: Parser Option
partOpt = Part <$> rpart <*> rof
  where
    rpart = option auto (long "part")
    rof = option auto (long "of")

onlyOpt :: Parser [String]
onlyOpt = some $ strOption $ long "only" <> short 'o' <> metavar "NAME" <> help "Benchmark only the function with NAME. Can be used multiple times" <> completeWith (map fst listOfSuites)

notOnlyOpt :: Parser [String]
notOnlyOpt = some $ strOption (long "notonly" <> short 'n' <> metavar "NAME" <> help "Do not benchmark function with NAME. Can be used multiple times")

libOpt :: Parser Lib
libOpt = strOption (long "lib" <> short 'l' <> metavar "LIBNAME" <> help "Benchmark only the library with LIBNAME. Can be used multiple times")

graphOpt :: Parser (Graph,Int)
graphOpt = option auto (long "graph" <> short 'g' <> metavar "GRAPH" <> help "graph to be tested (IGNORED FOR SPACE BENCHMARKS)")

graphsOpt :: Parser [(Graph,Int)]
graphsOpt = many graphOpt

options :: Parser Option
options = partOpt <|> (Only <$> onlyOpt)

sumFlag :: Parser Bool
sumFlag = flag True False $ long "noSummarize" <> short 's' <> help "When set, disable SUMMARIZE and ABSTRACT output"

saveOpt :: Parser String
saveOpt = strOption $ long "saveRawResults" <> short 'r' <> help "Write the raw results in a given file"

figFlag :: Parser (Maybe ChartOutput)
#ifdef CHART
figFlag = optional figSFlag
#else
figFlag = pure Nothing
#endif

figSFlag :: Parser ChartOutput
figSFlag = ChartOutput <$> outfile <*> outtype
  where
    outfile = strOption $ long "chartfile" <> short 'f' <> metavar "FILENAME" <> help "Output file WITHOUT extension" <> value "result"
    outtype = option auto $ long "chart" <> short 'c' <> metavar "OUTTYPE" <> help "Output type: Png or Svg"

benchWithCreation :: Parser Bool
benchWithCreation = flag False True $ long "bench-with-creation" <> short 'b' <> help "When set, will benchmark also the graph-creation function. See README (IGNORED FOR SPACE BENCHMARKS)"

benchLittleOne :: Parser Bool
benchLittleOne = flag False True $ long "dont-bench-little-ones" <> short 'i' <> help "When set, will only benchmark the largest graphs (IGNORED FOR SPACE BENCHMARKS)"

staFlag :: Parser StaOut
staFlag = option auto $ long "standardOutput" <> short 'd' <> value Ascii <> help ("The standard output, can be: " ++ intercalate ", " staOutCons) <> completeWith staOutCons

output :: Parser Output
output = Output <$> sumFlag <*> optional saveOpt <*> staFlag <*> figFlag

runCom :: Parser Command
runCom = Run <$> optional options <*> optional notOnlyOpt <*> output <*> optional (some libOpt) <*> benchWithCreation <*> benchLittleOne <*> graphsOpt

listOpt :: Parser ListOption
listOpt = flag' Benchs (long "benchs") <|> flag' Libs (long "libs")

listCom :: Parser Command
listCom = List <$> listOpt

renderGOpt :: Parser String
renderGOpt = strArgument $ help "the file to load raw datas. Likely something exported with \" run -r \"" <> metavar "INFILE"

renderGCom :: Parser Command
renderGCom = Render <$> renderGOpt <*> figSFlag

command' :: Parser Command
command' = subparser
  ( command "list" list
    <> command "run" run
    <> command "renderG" renderG
  )
  where
    run = info (runCom <**> helper)
      ( fullDesc
      <> progDesc "Compare benchmarks of graphs libraries"
      <> header "Help" )
    list = info (listCom <**> helper)
      ( fullDesc
     <> progDesc "List benchmarks"
     <> header "Help" )
    renderG = info (renderGCom <**> helper)
      ( fullDesc
     <> progDesc "Render data into a graph"
     <> header "Help" )

commandP :: ParserInfo Command
commandP = info (semiOptional <**> helper)
      ( fullDesc
     <> progDesc "Benchmark different graphs libraries"
     <> header "Help" )
  where
    semiOptional = pure (fromMaybe (Run Nothing Nothing (Output True Nothing Ascii Nothing) Nothing False False [])) <*> optional command'

commandDataSize :: ParserInfo CommandDataSize
commandDataSize = info ((RunD <$> graphsOpt) <**> helper)
     ( fullDesc
     <> progDesc "Benchmark datasize on different graphs representations"
     <> header "Help")

