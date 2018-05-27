{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Types (Grouped (..), lengthG, IsGrouped, tkSimple, tkGroup, isSimple )
where

import qualified Weigh as W

data Grouped a = Simple a | Group [Grouped a] deriving (Show)

instance Functor Grouped where
  fmap f (Simple a) = Simple $ f a
  fmap f (Group lst) = Group $ map (fmap f) lst

lengthG :: Grouped a -> Int
lengthG a = case a of
              Simple{} -> 1
              Group a' -> sum $ map lengthG a'

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
  simple_ (Simple e) = e
  group_ (Group e) = e

-- | Weigh Grouped isGrouped
instance IsGrouped W.Grouped where
  isSimple W.Singleton{} = True
  isSimple _ = False
  simple_ (W.Singleton e) = e
  group_ (W.Grouped _ e) = e

