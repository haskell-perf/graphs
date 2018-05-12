module BenchGraph.Named
  (
    Name,
    Named (..),
    WithName,
    getName,
    nameShow,
    nameBy,
    toNamed,
    showListN,
    classicShow
  )
where

type Name = String

class WithName a where
  getName :: a -> String

data Named a = Named {
  name :: Name,
  obj :: a
                     }

instance WithName (Named a) where
  getName = name

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

showListN :: WithName a => [Named a] -> String
showListN = unlines . map (getName . obj)
