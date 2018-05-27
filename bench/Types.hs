module Types (Grouped (..), lengthG, takeSimple, takeChilds)
where

data Grouped a = Simple a | Group [Grouped a] deriving (Show)

instance Functor Grouped where
  fmap f (Simple a) = Simple $ f a
  fmap f (Group lst) = Group $ map (fmap f) lst

lengthG :: Grouped a -> Int
lengthG a = case a of
              Simple{} -> 1
              Group a' -> sum $ map lengthG a'

takeSimple :: Grouped a -> Maybe a
takeSimple (Simple a) = Just a
takeSimple _ = Nothing

takeChilds :: Grouped a -> Maybe [Grouped a]
takeChilds (Group a) = Just a
takeChilds _ = Nothing
