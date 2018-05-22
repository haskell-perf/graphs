module Types (Grouped (..))
where

data Grouped a = Simple a | Group [Grouped a] deriving (Show)

instance Functor Grouped where
  fmap f (Simple a) = Simple $ f a
  fmap f (Group lst) = Group $ map (fmap f) lst
