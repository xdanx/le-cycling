{-# LANGUAGE DeriveDataTypeable #-}
module Cyclist where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans
import Data.Typeable
import System.Random

import ID
import Population
import Stats
import Utils

data Cyclist = Cyclist {uid :: Int,            -- Unique ID
                        pmax :: Double,       -- Max power (W/kg?)
                        pcp :: Double,        -- Maximum aneorbosfewnqgff power
                        usedEnergy :: Double, -- e_an : used aneorobic energy 
                        energyLim :: Double,  -- E_an : maximum aneorobic energy
                        genCProb :: Double,   -- General cooperation prob
                        teamCProb :: Double,  -- Team cooperation prob
                        genCoop :: Bool,      -- Current general cooperation state
                        teamCoop :: Bool,     -- Current team cooperation state
                        speed :: Double,      -- Current speed
                        distance :: Double,   -- Current distance
                        team :: Int           -- Team number.
                       }
                       deriving(Show, Typeable)

setPmax :: Cyclist -> Double -> Cyclist
setPmax c x = c{pmax = x}

setPcp :: Cyclist -> Double -> Cyclist
setPcp c x = c{pcp = x}

setUsedEnergy :: Cyclist -> Double -> Cyclist
setUsedEnergy c x = c{usedEnergy = x}

setEnergyLim :: Cyclist -> Double -> Cyclist
setEnergyLim c x = c{energyLim = x}

setGenCProb :: Cyclist -> Double -> Cyclist
setGenCProb c x = c{genCProb = x}

setTeamCProb :: Cyclist -> Double -> Cyclist
setTeamCProb c x = c{teamCProb = x}

setSpeed :: Cyclist -> Double -> Cyclist
setSpeed c x = c{speed = x}

setDistance :: Cyclist -> Double -> Cyclist
setDistance c x = c{distance = x}


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

maxPower :: Cyclist -> Double
maxPower c = pmax c * (1 - (usedEnergy c/energyLim c))

genCyclist :: Int -> Population -> RandT StdGen IO Cyclist
genCyclist team_n stats = do
           _pmax <- normal . pmaxs $ stats
           _genCProb <- normal . coops $ stats
           _teamCProb <- normal . coops $ stats
           _energyLim <- normal . energylims $ stats
           i <- newID
           return Cyclist {uid = i, pmax = _pmax, pcp = 0.8*_pmax, usedEnergy = 0, energyLim = _energyLim, genCProb = _genCProb, teamCProb = _teamCProb, speed = 0, distance = 0, team = team_n, teamCoop = True, genCoop = True}

genCyclists :: Int -> Int -> Population -> RandT StdGen IO [Cyclist]
genCyclists n_teams team_size stats = concatMapM (\t -> replicateM team_size (genCyclist t stats)) [0..(n_teams-1)]

genCyclistsIO :: Int -> Int -> Population -> IO [Cyclist]
genCyclistsIO n_teams team_size stats = do
              g <- getStdGen
              flip evalRandT g $ genCyclists n_teams team_size stats
