module Cyclist where

import Control.Monad.Random

data Cyclist = Cyclist {max10 :: Double, s_m :: Double, e_rem :: Double, c_b :: Double, c_t :: Double, breakaway :: Bool, speed :: Double, distance :: Double}
     deriving (Show)

instance Eq Cyclist where
         a == b = (distance a == distance b)
         a /= b  = (distance a /= distance b)

-- orders from last to first in the race.
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

genCyclist :: Rand StdGen Cyclist
genCyclist = do
           max10 <- getRandom
           s_m <- getRandom
           e_rem <- getRandom
           c_b <- getRandom
           c_t <- getRandom
           return Cyclist {max10 = max10, s_m = s_m, e_rem = e_rem, c_b = c_b, c_t = c_t, breakaway = False, speed = 0, distance = 0}
