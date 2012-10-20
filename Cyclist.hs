module Cyclist where

import Control.Monad
import Control.Monad.Random
import System.Random

import Population
import Stats

data Cyclist = Cyclist {max10 :: Double, s_m :: Double, e_rem :: Double, c_b :: Double, c_t :: Double, breakaway :: Bool, speed :: Double, distance :: Double, position :: Int}
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

genCyclist :: Population -> Rand StdGen Cyclist
genCyclist stats = do
           max10 <- normal . max10s $ stats
           c_b <- normal . coops $ stats
           c_t <- normal . coops $ stats
           return Cyclist {max10 = max10, s_m = exp 2.478, e_rem = (1/0), c_b = c_b, c_t = c_t, breakaway = False, speed = 0, distance = 0, position = 1}

genCyclists :: Int -> Population -> Rand StdGen [Cyclist]
genCyclists n stats = replicateM n (genCyclist stats)

genCyclistsIO :: Int -> Population -> IO [Cyclist]
genCyclistsIO n stats = evalRandIO $ genCyclists n stats 
