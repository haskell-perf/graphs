{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}

module BenchGraph.Render.Types

  ( Grouped (..)
  , lengthG
  , setBGroupT
  , setGName
  , IsGrouped (..)
  , ChartOutputFormat (..)
  , ChartOutput (..)
  )

where

import GHC.Generics
import Data.Aeson

-- | The Bool is here to tell if we get it into the benchs
-- The string is the type of graphs used
data Grouped a = Simple Bool String a | Group [Grouped a] deriving (Generic, Show, Read)

instance ToJSON a => ToJSON (Grouped a) where
    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Grouped a)

instance Functor Grouped where
  fmap f (Simple b n a) = Simple b n $ f a
  fmap f (Group lst) = Group $ map (fmap f) lst

lengthG :: Grouped a -> Int
lengthG a = case a of
              Simple{} -> 1
              Group a' -> sum $ map lengthG a'

setBGroupT :: Grouped a -> Grouped a
setBGroupT = setBGroup True

setBGroup :: Bool -> Grouped a -> Grouped a
setBGroup b (Simple _ n a) = Simple b n a
setBGroup b (Group lst@(Group (Simple{}:_):_)) = Group $ map (setBGroup (not b)) (init lst) ++ [setBGroup b $ last lst]
setBGroup b (Group lst) = Group $ map (setBGroup b) lst

setGName :: String -> Grouped a -> Grouped a
setGName s (Simple b _ xs) = Simple b s xs
setGName s (Group xs) = Group $ map (setGName s) xs

class IsGrouped f where
  isSimple :: f a -> Bool
  simple_ :: f a -> a
  group_ :: f a -> [f a]

  tkSimple :: f a -> Maybe a
  tkSimple e = if isSimple e then Just (simple_ e) else Nothing
  tkGroup :: f a -> Maybe [f a]
  tkGroup e = if not (isSimple e) then Just (group_ e) else Nothing

instance IsGrouped Grouped where
  isSimple Simple{} = True
  isSimple _ = False
  simple_ (Simple _ _ e) = e
  group_ (Group e) = e

data ChartOutput = ChartOutput String ChartOutputFormat deriving (Read, Show, Eq)
data ChartOutputFormat = Png | Svg deriving (Read, Show, Eq)
