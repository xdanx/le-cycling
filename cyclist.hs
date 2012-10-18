module Cyclist where

data Cyclist = Cyclist {max10 :: Double, distance :: Double}

instance Eq Cyclist where
         a == b = (distance a == distance b)
         a /= b  = (distance a /= distance b)

instance Ord Cyclist where
         compare a b = compare (distance a) (distance b)
         a < b = (distance a) < (distance b)
         a <= b = (distance a) <= (distance b)
         a >= b = (distance a) >= (distance b)
         a > b = (distance a) > (distance b)
         max a b
             | a < b = b
             | otherwise = a
         min a b
             | a < b = a
             | otherwise = b
