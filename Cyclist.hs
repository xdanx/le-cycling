module Cyclist where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans
import System.Random

import Utils
import Population
import Stats

data Cyclist = Cyclist {id :: Int,           -- Unique ID
                        max10 :: Double,     -- 10-min Max power (W/kg)
                        speedM10 :: Double,  -- Speed at max10 power output (m/s)
                        tExh :: Double,      -- Time until exhaustion (by Tlim)
                        genCProb :: Double,  -- General cooperation prob
                        teamCProb :: Double, -- Team cooperation prob
                        genCoop :: Bool,     -- Current general cooperation state
                        teamCoop :: Bool,    -- Current team cooperation state
                        breakaway :: Int,    -- Breakaway state (0 : not breakaway, n > 0 breakaway
                                             -- for n minutes or until they catch another pack )
                        speed :: Double,     -- Current speed
                        distance :: Double,  -- Current distance
                        team :: Int          -- Team number.
                       }
                       deriving(Show)

{-instance Show Cyclist where
         show c = "distance: " ++ (show . distance $ c) ++ ", id: " ++ (show . Cyclist.id $ c)-}

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

genCyclist :: Int -> Population -> RandT StdGen (StateT Int IO) Cyclist
genCyclist team_n stats = do
           _max10 <- normal . max10s $ stats
           _genCProb <- normal . coops $ stats
           _teamCProb <- normal . coops $ stats
           i <- lift get
           lift . put $ (i + 1)
           return Cyclist {Cyclist.id = i, max10 = _max10, speedM10 = exp 2.478, tExh = (1/0), genCProb = _genCProb, teamCProb = _teamCProb, breakaway = 0, speed = 0, distance = 0, team = team_n, teamCoop = True, genCoop = True}

genCyclists :: Int -> Int -> Population -> RandT StdGen (StateT Int IO) [Cyclist]
genCyclists n_teams team_size stats = concatMapM (\t -> replicateM team_size (genCyclist t stats)) [0..(n_teams-1)]

genCyclistsIO :: Int -> Int -> Population -> IO [Cyclist]
genCyclistsIO n_teams team_size stats = do
              g <- getStdGen
              flip evalStateT 0 . flip evalRandT g $ genCyclists n_teams team_size stats
