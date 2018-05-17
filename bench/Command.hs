module Command
  (
  Option (..),
  Flag (..),
  Command (..),
  commandTime,
  flagSpace
  )

where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

data Option = Part Int Int | Only String
  deriving (Show, Eq)

data Flag = Summarize
  deriving (Show, Eq)

data Command = List | Run (Maybe Option) (Maybe Flag)
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

options :: Parser Option
options = partOpt <|> onlyOpt

sumFlag :: Parser Flag
sumFlag = flag' Summarize $ long "summarize" <> short 's'

runCom :: Parser Command
runCom = Run <$> optional options <*> optional sumFlag

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
commandTime = info (semiOptional <**> helper)
      ( fullDesc
     <> progDesc "Benchmark time of functions on different graphs libraries"
     <> header "Help" )
  where
    semiOptional = pure (fromMaybe (Run Nothing Nothing)) <*> optional command'

flagSpace :: ParserInfo (Maybe Flag)
flagSpace = info (optional sumFlag <**> helper)
      ( fullDesc
     <> progDesc "Benchmark size of functions on different graphs libraries"
     <> header "Help" )

