{-# LANGUAGE RankNTypes #-}
module Cyclist where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans
import Data.Typeable
import System.Random

import Coop
import ID
import Population
import Stats
import Utils
import Units

data Cyclist = Cyclist {uid :: Int,            -- Unique ID
                        pmax :: Double,       -- Max power (W/kg?)
                        pcp :: Double,        -- Maximum aneorbosfewnqgff power
                        usedEnergy :: Double, -- e_an : used aneorobic energy 
                        energyLim :: Double,  -- E_an : maximum aneorobic energy
                        packCoop :: RandT StdGen IO Bool,   -- General cooperation function
                        teamCoop :: RandT StdGen IO Bool,  -- Team cooperation function
                        groupProb :: Double,
                        teamProb :: Double,
                        speed :: Double,      -- Current speed
                        distance :: Double,   -- Current distance
                        team :: Int           -- Team number.
                       }

instance Show Cyclist where
        show c = "Cyclist {uid = " ++ show (uid c) ++ "}"

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

setSpeed :: Cyclist -> Speed -> Cyclist
setSpeed c x = c{speed = x}

setDistance :: Cyclist -> Meters -> Cyclist
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
           _groupProb <- normal . coops $ stats
           _teamProb <- normal . coops $ stats
           _energyLim <- normal . energylims $ stats
           i <- newID
           return Cyclist {uid = i, pmax = _pmax, pcp = 0.8*_pmax, usedEnergy = 0, energyLim = _energyLim, packCoop = standardCoop _groupProb, teamCoop = standardCoop _teamProb, groupProb = _groupProb, teamProb = _teamProb, speed = 0, distance = 0, team = team_n}

genCyclists :: Int -> Int -> Population -> RandT StdGen IO [Cyclist]
genCyclists n_teams team_size stats = concatMapM (\t -> replicateM team_size (genCyclist t stats)) [0..(n_teams-1)]

genCyclistsIO :: Int -> Int -> Population -> IO [Cyclist]
genCyclistsIO n_teams team_size stats = do
              g <- getStdGen
              flip evalRandT g $ genCyclists n_teams team_size stats
