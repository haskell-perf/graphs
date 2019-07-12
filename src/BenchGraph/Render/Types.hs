{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}

module BenchGraph.Render.Types

  ( Grouped (..)
  , lengthG
  , removeTailLast
  , setGName
  , tkGroup
  , tkSimple
  , ChartOutput (..)
  )

where

import GHC.Generics
import Data.Aeson

-- |  The string is the type of graphs used
data Grouped a = Simple String a | Group [Grouped a] deriving (Generic, Show, Read)

instance ToJSON a => ToJSON (Grouped a) where
    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Grouped a)

instance Functor Grouped where
  fmap f (Simple n a) = Simple n $ f a
  fmap f (Group lst) = Group $ map (fmap f) lst

tkGroup :: Grouped a -> Maybe [Grouped a]
tkGroup (Group lst) = Just lst
tkGroup _ = Nothing

tkSimple :: Grouped a -> Maybe a
tkSimple (Simple _ a) = Just a
tkSimple _ = Nothing

lengthG :: Grouped a -> Int
lengthG a = case a of
              Simple{} -> 1
              Group a' -> sum $ map lengthG a'

removeTailLast :: Grouped a -> Grouped a
removeTailLast s@Simple{} = s
removeTailLast (Group lst@(Group (Simple{}:_):_)) = Group $ [last lst]
removeTailLast (Group lst) = Group $ map removeTailLast lst

setGName :: String -> Grouped a -> Grouped a
setGName s (Simple _ xs) = Simple s xs
setGName s (Group xs) = Group $ map (setGName s) xs

data ChartOutput = ChartOutput String deriving (Read, Show, Eq)
