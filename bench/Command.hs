module Command
  (
  Option (..),
  Flag (..),
  Command (..),
  commandTime,
  runSpace
  )

where

import Options.Applicative
import Data.Semigroup ((<>))

data Option = Part Int Int | Only String
  deriving (Show, Eq)

type Size = (Int,Int,Int)

data Flag = Summarize
  deriving (Show, Eq)

data Command = List | Run (Maybe Option) (Maybe Flag) Size
  deriving (Show, Eq)

listCom :: Parser Command
listCom = pure List

partOpt :: Parser Option
partOpt = Part <$> rpart <*> rof
  where
    rpart = option auto (long "part")
    rof = option auto (long "of")

onlyOpt :: Parser Option
onlyOpt = Only <$> strOption (long "only" <> short 'o' <> metavar "NAME")

sizeOpt :: Parser Size
sizeOpt = read <$> strOption (long "graphs-size" <> value "(5,3,3)" <> showDefault <> help "(ten power to generate path, ten power to generate a circuit, ten power to generate a complete graph)" )

options :: Parser Option
options = partOpt <|> onlyOpt

sumFlag :: Parser Flag
sumFlag = flag' Summarize $ long "summarize" <> short 's'

runCom :: Parser Command
runCom = Run <$> optional options <*> optional sumFlag <*> sizeOpt

command' :: Parser Command
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
     <> progDesc "Compare benchmarks of graphs libraries"
     <> header "Help" )

commandTime :: ParserInfo Command
commandTime = info (command' <**> helper)
      ( fullDesc
     <> progDesc "Benchmark time of functions on different graphs libraries"
     <> header "Help" )

runSpace :: ParserInfo (Maybe Flag)
runSpace = info (optional sumFlag <**> helper)
      ( fullDesc
     <> progDesc "Benchmark size of functions on different graphs libraries"
     <> header "Help" )

