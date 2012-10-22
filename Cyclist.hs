module Cyclist where

import Control.Monad
import Control.Monad.Random
import System.Random

import Utils
import Population
import Stats

data Cyclist = Cyclist {max10 :: Double,    -- 10-min Max power (W/kg) 
                        s_m :: Double,      -- Speed at max10 power output (m/s)
                        e_rem :: Double,    -- Time until exhaustion (by Tlim)
                        c_b :: Double,      -- Cooperation prob
                        c_t :: Double,      -- Team cooperation prob
                        breakaway :: Int,   -- Breakaway state (0 : not breakaway, n > 0 breakaway
                                            -- for n minutes or until they catch another pack )
                        speed :: Double,   
                        distance :: Double,
                        position :: Int,
                        t_lead :: Int,      -- Time spend at lead position in pack.   
                        team :: Int,         -- Team number.
                        t_coop :: Bool,
                        b_coop :: Bool
                       }
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

genCyclist :: Int -> Population -> Rand StdGen Cyclist
genCyclist team_n stats = do
           max10 <- normal . max10s $ stats
           c_b <- normal . coops $ stats
           c_t <- normal . coops $ stats
           return Cyclist {max10 = max10, s_m = exp 2.478, e_rem = (1/0), c_b = c_b, c_t = c_t, breakaway = 0, speed = 0, distance = 0, position = 1, t_lead = 0, team = team_n, t_coop = True, b_coop = True}

genCyclists :: Int -> Int -> Population -> Rand StdGen [Cyclist]
genCyclists n_teams team_size stats = concatMapM (\t -> (replicateM team_size (genCyclist t stats))) [1..n_teams]

genCyclistsIO :: Int -> Int -> Population -> IO [Cyclist]
genCyclistsIO n_teams team_size stats = evalRandIO $ genCyclists n_teams team_size stats 
