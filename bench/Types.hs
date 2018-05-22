module Types (Grouped (..), lengthG)
where

data Grouped a = Simple a | Group [Grouped a] deriving (Show)

instance Functor Grouped where
  fmap f (Simple a) = Simple $ f a
  fmap f (Group lst) = Group $ map (fmap f) lst

lengthG :: Grouped a -> Int
lengthG a = case a of
              Simple{} -> 1
              Group a' -> sum $ map lengthG a'
