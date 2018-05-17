module BenchGraph.Named
  (
    Name,
    Named (..),
    nameBy,
    toNamed,
    fromNamed,
    nameShow,
    liftExtract,
    liftExtract2
  )
where

import Control.Comonad
import Data.Functor.Classes

type Name = String

-- | The Named data, a Name and the data
data Named a = Named Name a

-- | Show is defined by the name of the object
instance Show (Named a) where
  show (Named name _) = name

instance Functor Named where
  fmap = liftW

-- | Named is a CoMonad
instance Comonad Named where
  extract (Named _ obj) = obj
  extend f named = Named (show named) $ f named

instance Foldable Named where
  foldMap = liftExtract

-- | You can sequence a name
instance Traversable Named where
  sequenceA (Named n m) = Named n <$> m

-- | Named are equals if their data are
instance Eq a => Eq (Named a) where
  (==) = liftExtract2 (==)

instance Ord a => Ord (Named a) where
  (<=) = liftExtract2 (<=)

nameShow :: Show a => a -> Named a
nameShow = nameBy show

nameBy :: (a -> Name) -> a -> Named a
nameBy f a = Named (f a) a

toNamed :: (Name,a) -> Named a
toNamed = uncurry Named

fromNamed :: Named a -> (Name,a)
fromNamed (Named n a) = (n,a)

-- | Lifting functions in comonads
liftExtract :: (Comonad w) => (a -> b) -> w a -> b
liftExtract f = f . extract

liftExtract2 :: (Comonad w) => (a-> b -> c) -> w a -> w b -> c
liftExtract2 f a b = f (extract a) (extract b)
