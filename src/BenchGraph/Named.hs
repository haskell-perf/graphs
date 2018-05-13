module BenchGraph.Named
  (
    Name,
    Named (..),
    nameBy,
    toNamed,
    classicShow,
    fromNamed,
    nameShow
  )
where

type Name = String

data Named a = Named {
  name :: Name,
  obj :: a
                     }

instance Eq a => Eq (Named a) where
  a == b = obj a == obj b

instance Ord a => Ord (Named a) where
  a <= b = obj a <= obj b

instance Show (Named a) where
  show = name

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

fromNamed :: Named a -> (String,a)
fromNamed (Named n a) = (n,a)
