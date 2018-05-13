module BenchGraph.Named
  (
    Name,
    Named (..),
    nameBy,
    toNamed,
    classicShow,
    fromNamed,
    nameShow,
    invertMonad,
    fmapInM
  )
where

import Control.Comonad

type Name = String

data Named a = Named Name a

instance Show (Named a) where
  show (Named name _) = name

instance Functor Named where
  fmap = liftW

instance Comonad Named where
  extract (Named _ obj) = obj
  extend f named = Named (show named) $ f named

instance Eq a => Eq (Named a) where
  a == b = extract a == extract b

instance Ord a => Ord (Named a) where
  a <= b = extract a <= extract b

nameShow :: Show a => a -> Named a
nameShow = nameBy show

nameBy :: (a -> String) -> a -> Named a
nameBy f a = Named (f a) a

toNamed :: (String,a) -> Named a
toNamed = uncurry Named

classicShow :: Show a => Named a -> String
classicShow = show . extract

fromNamed :: Named a -> (String,a)
fromNamed (Named n a) = (n,a)

invertMonad :: Monad m => Named (m a) -> m (Named a)
invertMonad (Named n m) = Named n <$> m

fmapInM :: Monad m => (a -> m b) -> Named a -> m (Named b)
fmapInM f = invertMonad . fmap f
