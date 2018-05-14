module BenchGraph.Named
  (
    Name,
    Named (..),
    nameBy,
    toNamed,
    fromNamed,
    nameShow
  )
where

import Control.Comonad
import Data.Functor.Classes

type Name = String

data Named a = Named Name a

instance Show (Named a) where
  show (Named name _) = name

instance Show1 Named where
  liftShowsPrec f _ i = f i . extract

instance Functor Named where
  fmap = liftW

instance Comonad Named where
  extract (Named _ obj) = obj
  extend f named = Named (show named) $ f named

instance Foldable Named where
  foldMap f x = f $ extract x

instance Traversable Named where
  sequenceA (Named n m) = Named n <$> m

instance Eq a => Eq (Named a) where
  a == b = extract a == extract b

instance Ord a => Ord (Named a) where
  a <= b = extract a <= extract b

nameShow :: Show a => a -> Named a
nameShow = nameBy show

nameBy :: (a -> Name) -> a -> Named a
nameBy f a = Named (f a) a

toNamed :: (Name,a) -> Named a
toNamed = uncurry Named

fromNamed :: Named a -> (Name,a)
fromNamed (Named n a) = (n,a)

