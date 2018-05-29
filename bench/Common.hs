module Common

where

-- | Make the difference of two polynomials (used for chromatic number)
substractPoly :: [Int] -> [Int] -> [Int]
substractPoly (x:xs) (x':xs') = x-x' : substractPoly xs xs'
substractPoly [] (x:xs) = -x : substractPoly [] xs
substractPoly (x:xs) [] = x : substractPoly xs []
substractPoly [] [] = []
