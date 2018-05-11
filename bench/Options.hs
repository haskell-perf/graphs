module Options
  (
  Option (..),
  optionsI
  )

where

import Options.Applicative
import Data.Semigroup ((<>))

data Option = List | Part Int Int | Only String deriving (Show,Eq)

listOpt :: Parser Option
listOpt = flag' List (long "list" <> short 'l' <> help "List all benchmarks")

partOpt :: Parser Option
partOpt = Part <$> rpart <*> rof
  where
    rpart = option auto (long "part")
    rof = option auto (long "of")

onlyOpt :: Parser Option
onlyOpt = Only <$> strOption (long "only" <> short 'o')

options :: Parser (Maybe Option)
options = optional $ listOpt <|> partOpt <|> onlyOpt


optionsI :: ParserInfo (Maybe Option)
optionsI = info (options <**> helper)
      ( fullDesc
     <> progDesc "Compare benchmarks of graphs libraries"
     <> header "Help" )
