module Command
  (
  Option (..),
  Output (..),
  CommandTime (..),
  ListOption (..),
  CommandSpace (..),
  StaOut (..),
  CommandDataSize (..),
  commandTime,
  runSpace,
  runDataSize
  )

where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

data Option = Part Int Int | Only Only
  deriving (Show, Eq)

data StaOut = Ascii | Html | Null | QuickComparison deriving (Read, Show, Eq)

data Output = Output {
  sumOut :: Bool, -- ^ Output summary ?
  staOut :: StaOut -- ^ Output standard ?
  }
  deriving (Read, Show, Eq)

type Only = [String]
type Lib = String
type Graph = String

data ListOption = Benchs | Libs
  deriving (Show, Eq)

data CommandTime = List ListOption | Run (Maybe Option) Output (Maybe [Lib]) Bool Bool [(Graph,Int)]
  deriving (Show, Eq)

data CommandSpace = ListS ListOption | RunS (Maybe Only) Output (Maybe [Lib])

newtype CommandDataSize = RunD [(Graph,Int)]

partOpt :: Parser Option
partOpt = Part <$> rpart <*> rof
  where
    rpart = option auto (long "part")
    rof = option auto (long "of")

onlyOpt :: Parser [String]
onlyOpt = some $ strOption (long "only" <> short 'o' <> metavar "NAME" <> help "Benchmark only the function with NAME. Can be used multiple times")

libOpt :: Parser Lib
libOpt = strOption (long "lib" <> short 'l' <> metavar "LIBNAME" <> help "Benchmark only the library with LIBNAME. Can be used multiple times")

graphOpt :: Parser (Graph,Int)
graphOpt = option auto (long "graph" <> short 'g' <> metavar "GRAPH" <> help "graph to be tested")

graphsOpt :: Parser [(Graph,Int)]
graphsOpt = many graphOpt

options :: Parser Option
options = partOpt <|> ( Only <$> onlyOpt)

sumFlag :: Parser Bool
sumFlag = flag True False $ long "noSummarize" <> short 's' <> help "When set, disable SUMMARIZE and ABSTRACT output"

benchWithCreation :: Parser Bool
benchWithCreation = flag False True $ long "bench-with-creation" <> short 'b' <> help "When set, will benchmark also the graph-creation function. See README"

benchLittleOne :: Parser Bool
benchLittleOne = flag False True $ long "dont-bench-little-ones" <> short 'i' <> help "When set, will only benchmark the largest graphs"

staFlag :: Parser StaOut
staFlag = option auto $ long "standardOutput" <> short 'd' <> value Ascii <> help "The standard output, can be: Ascii | Html | Null | QuickComparison"

output :: Parser Output
output = Output <$> sumFlag <*> staFlag

runCom :: Parser CommandTime
runCom = Run <$> optional options <*> output <*> optional (some libOpt) <*> benchWithCreation <*> benchLittleOne <*> graphsOpt

listOpt :: Parser ListOption
listOpt = flag' Benchs (long "benchs") <|> flag' Libs (long "libs")

listCom :: Parser CommandTime
listCom = List <$> listOpt

command' :: Parser CommandTime
command' = subparser
  ( command "list" list
    <> command "run" run
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

commandTime :: ParserInfo CommandTime
commandTime = info (command' <**> helper)
      ( fullDesc
     <> progDesc "Benchmark time of functions on different graphs libraries"
     <> header "Help" )

space' :: Parser CommandSpace
space' = subparser
  ( command "list" list
    <> command "run" run
  )
  where
    list = info (ListS <$> listOpt <**> helper)
      ( fullDesc
      <> progDesc "Compare benchmarks of graphs libraries"
      <> header "Help" )
    run = info ( (RunS <$> optional onlyOpt <*> output <*> optional (some libOpt)) <**> helper)
      ( fullDesc
     <> progDesc "list benchmarks"
     <> header "Help" )

runSpace :: ParserInfo CommandSpace
runSpace = info ( semiOptional <**> helper)
      ( fullDesc
     <> progDesc "Benchmark size of functions on different graphs libraries"
     <> header "Help")
  where
    semiOptional = pure (fromMaybe (RunS Nothing (Output True Ascii) Nothing)) <*> optional space'

runDataSize :: ParserInfo CommandDataSize
runDataSize = info ((RunD <$> graphsOpt) <**> helper)
     ( fullDesc
     <> progDesc "Benchmark datasize on different graphs representations"
     <> header "Help")

