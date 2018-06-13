{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module BenchGraph.Render.Types
  ( Grouped (..)
  , lengthG
  , IsGrouped
  , tkSimple
  , tkGroup
  , isSimple
  , setBGroup
  )
where

import qualified Weigh as W

-- | The Bool is here to tell if we get it into the benchs
data Grouped a = Simple Bool a | Group [Grouped a] deriving (Show)

instance Functor Grouped where
  fmap f (Simple b a) = Simple b $ f a
  fmap f (Group lst) = Group $ map (fmap f) lst

lengthG :: Grouped a -> Int
lengthG a = case a of
              Simple{} -> 1
              Group a' -> sum $ map lengthG a'

setBGroup :: Bool -> Grouped a -> Grouped a
setBGroup b (Simple _ a) = Simple b a
setBGroup b (Group lst@(Group (Simple True _:_):_)) = Group $ map (setBGroup (not b)) (init lst) ++ [setBGroup b $ last lst]
setBGroup b (Group lst) = Group $ map (setBGroup b) lst


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
  simple_ (Simple _ e) = e
  group_ (Group e) = e

-- | Weigh Grouped isGrouped
instance IsGrouped W.Grouped where
  isSimple W.Singleton{} = True
  isSimple _ = False
  simple_ (W.Singleton e) = e
  group_ (W.Grouped _ e) = e

