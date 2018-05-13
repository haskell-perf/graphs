module BenchGraph.Named
  (
    Name,
    Named (..),
    nameShow,
    nameBy,
    toNamed,
    classicShow
  )
where

type Name = String

data Named a = Named {
  name :: Name,
  obj :: a
                     }

instance Eq a => Eq (Named a) where
  (Named _ a ) == (Named _ b) = a == b

instance Ord a => Ord (Named a) where
  a <= b = obj a <= obj b

instance Show (Named a) where
  show (Named a _) = a

instance Functor Named where
  fmap f (Named n x) = Named n (f x)

nameShow :: Show a => a -> Named a
nameShow = nameBy show

nameBy :: (a -> String) -> a -> Named a
nameBy f a = Named (f a) a

toNamed :: (String,a) -> Named a
toNamed = uncurry Named

classicShow :: Show a => Named a -> String
classicShow = show . obj
