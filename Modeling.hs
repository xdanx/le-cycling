{- Modeling.hs
Functions that have to do with the model of speed and energy.
Based on the second paper :
[Applications of Mathematical Models of Road Cycling 
by Thorsten Dahmen, Stefan Wolf and Dietmar Saupe]
-}

module Modeling where

import Data.Foldable as Fold
import Data.Sequence as Sequence

import Cyclist
import Pack
import RungeKutta
import Units

-- Updates the speed of the cyclists in a pack
-- Creates new packs if some cyclists are too weak to follow the rest
setPackSpeed :: Pack -> Pack
setPackSpeed pack = packMap (\c -> updateCyclistPhysics c (min (pm c) avgPped)) pack
  where
    avgPped = coef * (avgpmax (getPack pack))
    coef = if isBreak pack then 0.9 else 0.8

-- Average pmax of a sequence of cyclists
avgpmax :: (Seq Cyclist) -> Double
avgpmax p = (Fold.foldl (+) 0 (fmap pmax p)) / (fromIntegral $ Sequence.length p)

-- Updates the speed of a single sprinter cyclist
setSprinterSpeed :: Cyclist -> Cyclist
setSprinterSpeed c = updateCyclistPhysics c (min (pm c) (0.95 * (pmax c)))

-- Update the used energy of a cyclists, depending on his speed
-- Need to do something different if it's in_pack vs (leader, breakaway or sprint)

updatePackEnergy :: Pack -> Pack
updatePackEnergy (Pack tLead l p pid) = (Pack tLead (updateEnergy True l) (fmap (updateEnergy False) p) pid)
updatePackEnergy (Breakaway p t pid) = (Breakaway ((updateEnergy True l)<|(fmap (updateEnergy False) p)) t pid)
  where
    (l:<p') = viewl p

updateEnergy :: Bool -> Cyclist -> Cyclist
updateEnergy lead c = c{usedEnergy = min (energyLim c) . max 0 $ (usedEnergy c) + (fromIntegral tick) * ((pped lead c) - (pcp c))}

pped :: Bool -> Cyclist -> Double
pped lead c = 108.237684393 * (0.62 - 0.0104*d_w + 0.0452*d_w^2) * spd^3 + 14844.025288499999 * spd * acc
     where spd = speed c
           acc = acceleration c
           d_w = if lead then 3 else 1.5


-- Gives the current maximum power output of a cyclist,
pm :: Cyclist -> Double
pm c =  (pmax c) * (1 - ((usedEnergy c)/(energyLim c)))
