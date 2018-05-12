module BenchGraph.Named
  (
    Name,
    Named (..),
    WithName,
    getName,
    nameShow,
    nameBy,
    toNamed,
    showListN
  )
where

import Data.Functor.Classes

type Name = String

class WithName a where
  getName :: a -> String

showListN :: WithName a => [a] -> String
showListN = unlines . map getName

data Named a = Named {
  name :: Name,
  obj :: a
                     }

instance WithName (Named a) where
  getName = name

instance Eq (Named a) where
  (Named a _ ) == (Named b _) = a == b

instance Eq1 Named where
  liftEq f (Named _ a) (Named _ b) = f a b

instance Show (Named a) where
  show (Named a _) = a

instance Ord a => Ord (Named a) where
  a <= b = obj a <= obj b

instance Functor Named where
  fmap f (Named n x) = Named n (f x)

nameShow :: Show a => a -> Named a
nameShow = nameBy show

nameBy :: (a -> String) -> a -> Named a
nameBy f a = Named (f a) a

toNamed :: (String,a) -> Named a
toNamed = uncurry Named
