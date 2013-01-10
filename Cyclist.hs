{-# LANGUAGE RankNTypes #-}

{- Cyclist.hs
Defines the Cyclist type which represent a single Cyclist
and its state in the race.
-}

module Cyclist where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans
import Data.Typeable
import System.Random

import Coop
import Population
import Stats
import Units
import Utils

data Cyclist = Cyclist {uid :: Int,           -- C Unique ID
                        pmax :: Double,       -- C Max power (W/kg?)
                        pcp :: Double,        -- C Maximum aneorbosfewnqgff power
                        usedEnergy :: Double, -- e_an : used aneorobic energy 
                        energyLim :: Double,  -- C E_an : maximum aneorobic energy
                        packCoop :: RandT StdGen IO Bool, -- General cooperation function
                        teamCoop :: RandT StdGen IO Bool, -- Team cooperation function
                        groupProb :: Double,  -- If the default cooperation function is 
                        teamProb :: Double,   -- used, these stores the probabilities.
                        speed :: Speed,       -- Current speed
                        distance :: Meters,   -- Current distance
                        acceleration :: Acceleration, -- Current acceleration
                        team :: Int           -- C Team number.
                       }

instance Show Cyclist where
        show c = "Cyclist {uid = " ++ show (uid c) ++ ", distance: " ++ show (distance c) ++ ", speed: " ++ show (speed c) ++ "}"

setPmax :: Cyclist -> Double -> Cyclist
setPmax c x = c{pmax = x}

setPcp :: Cyclist -> Double -> Cyclist
setPcp c x = c{pcp = x}

setUsedEnergy :: Cyclist -> Double -> Cyclist
setUsedEnergy c x = c{usedEnergy = x}

setEnergyLim :: Cyclist -> Double -> Cyclist
setEnergyLim c x = c{energyLim = x}

setPackCoop:: Cyclist -> RandT StdGen IO Bool -> Cyclist
setPackCoop c x = c{packCoop = x}

setTeamCoop :: Cyclist -> RandT StdGen IO Bool -> Cyclist
setTeamCoop c x = c{teamCoop = x}

sanitiseSpeed :: Speed -> Speed
sanitiseSpeed x = if(x < 1)
                  then 1
                  else x

setSpeed :: Cyclist -> Speed -> Cyclist
setSpeed c x = c{speed = sanitiseSpeed x}

setDistance :: Cyclist -> Meters -> Cyclist
setDistance c x = c{distance = x}

-- Shouldn't Eq be defined with the ID instead ?
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

-- Gives the current maximum power output of a cyclist,
maxPower :: Cyclist -> Double
maxPower c = pmax c * (1 - (usedEnergy c/energyLim c))


-- Functions to generate the cyclists to start the race.

-- Generate one cyclists with the default coop function, the default distribution,
-- stats from stats and in team team_n
-- !! Need updating to take the function in the parameters !!
genCyclist :: Int -> ((Double -> RandT StdGen IO Bool), Population) -> RandT StdGen IO Cyclist
genCyclist team_n (strat, distr) = do
           _pmax <- normal . pmaxs $ distr
           _groupProb <- normal . coops $ distr
           _teamProb <- normal . coops $ distr
           _energyLim <- normal . energylims $ distr
           i <- newID
           return Cyclist {uid = i, pmax = _pmax, pcp = 0.8*_pmax, usedEnergy = 0, energyLim = _energyLim, packCoop = strat _groupProb, teamCoop = strat _teamProb, groupProb = _groupProb, teamProb = _teamProb, speed = 1, acceleration = 0.000053894*_pmax, distance = 0, team = team_n}

-- Generate team_size cyclists with distr distr for each
-- team between 0 and n_teams.
genCyclists :: Int -> Int -> ((Double -> RandT StdGen IO Bool), Population) -> RandT StdGen IO [Cyclist]
genCyclists n_teams team_size (strat, distr) = concatMapM (\t -> replicateM team_size (genCyclist t (strat, distr))) [0..(n_teams-1)]

genCyclistsIO :: Int -> Int -> ((Double -> RandT StdGen IO Bool), Population) -> IO [Cyclist]
genCyclistsIO n_teams team_size (strat, distr) = do
              g <- getStdGen
              flip evalRandT g $ genCyclists n_teams team_size (strat, distr)
