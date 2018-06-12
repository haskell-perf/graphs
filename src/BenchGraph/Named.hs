module BenchGraph.Named
  (
    Name,
    Named,
    eq1,
    compare1,
    nameBy,
    nameShow,
    withNames,
    liftExtract2,
    fix
  )
where

import Control.Comonad

type Name = String

-- | The Named type
type Named a = (Name,a)

-- | Named are equals if their data are
eq1 :: Eq a => Named a -> Named a -> Bool
eq1 = liftExtract2 (==)

compare1 :: Ord a => Named a -> Named a -> Ordering
compare1 = liftExtract2 compare

-- Show items in a list
withNames :: Show a => [a] -> [Named a]
withNames = map nameShow

nameShow :: Show a => a -> Named a
nameShow = nameBy show

nameBy :: (a -> Name) -> a -> Named a
nameBy f a = (f a, a)

liftExtract2 :: (Comonad w) => (a -> b -> c) -> w a -> w b -> c
liftExtract2 f a b = f (extract a) (extract b)

-- | Precedence for the first name
fix :: Named (Named a) -> Named a
fix (n ,(_, a)) = (n,a)
