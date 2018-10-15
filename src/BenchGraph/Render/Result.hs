{-# LANGUAGE DeriveGeneric #-}

module BenchGraph.Render.Result (Result (..)) where

import BenchGraph.Render.Types
import BenchGraph.Named

import Data.Aeson hiding (Result)
import GHC.Generics

data Result a = Result
  { graphsArgs   :: [(String,Int)]
  , benchResults :: [Named (Grouped a)]
  } deriving (Generic)

instance ToJSON a => ToJSON (Result a) where
    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Result a)

