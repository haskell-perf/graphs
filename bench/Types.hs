module Types (Grouped (..))
where

data Grouped a = Simple a | Group [Grouped a] deriving (Show)
