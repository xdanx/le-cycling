{- Modeling.hs
Functions that have to do with the model of speed and energy.
Based on the second paper :
[Applications of Mathematical Models of Road Cycling 
by Thorsten Dahmen, Stefan Wolf and Dietmar Saupe]
-}

module Modeling where

import Data.Foldable as Fold
import Data.Sequence as Sequence
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans
import System.Exit

import Cyclist
import Pack
import RungeKutta

-- Updates the speed of the cyclists in a pack
-- Creates new packs if some cyclists are too weak to follow the rest
setPackSpeed :: Pack -> Pack
setPackSpeed pack = packMap (\c -> updateCyclistPhysics c (max (pm c) avgPped)) pack
  where
    avgPped = coef * (avgpmax (getPack pack))
    coef = if isBreak pack then 0.9 else 0.8

-- Average pmax of a sequence of cyclists
avgpmax :: (Seq Cyclist) -> Double
avgpmax p = (Fold.foldl (+) 0 (fmap pmax p)) / (fromIntegral $ Sequence.length p)

-- Updates the speed of a single sprinter cyclist
setSprinterSpeed :: Cyclist -> Cyclist
setSprinterSpeed c = updateCyclistPhysics c (0.95 * (pmax c))

-- Update the used energy of a cyclists, depending on his speed
-- Need to do something different if it's in_pack vs (leader, breakaway or sprint)
updateEnergy :: Cyclist -> Cyclist
updateEnergy c = c{usedEnergy = (usedEnergy c) + 60 * ((pped c) - (pcp c))}

pped :: Cyclist -> Double
pped c = 75.7664 * spd^3 + 14844.025288499999 * spd * acc
     where spd = speed c
           acc = acceleration c

pm :: Cyclist -> Double
pm c = (pmax c) * (1 - ((usedEnergy c)/(energyLim c)))
