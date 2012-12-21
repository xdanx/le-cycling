module Modeling where

import Data.Foldable as Fold
import Data.Sequence as Sequence

import Cyclist
import Pack

setPackSpeed :: Pack -> Pack
setPackSpeed pack@(Pack tLead l p pid) = packMap (\c -> c{speed = newSpeed}) pack
  where
    newSpeed = 1.76777 * ppped * (tanh ((atanh (0.565685*(speed l) / ppped)) + 0.538748*ppped))
    ppped = sqrt (0.8 * (avgpmax (l <| p)))
setPackSpeed pack@(Breakaway p t pid) = packMap (\c -> c{speed = newSpeed}) pack
  where 
    newSpeed = 1.76777 * bpped * (tanh ((atanh (0.565685*(speed someCyclist) / bpped)) + 0.538748*bpped))
    bpped = sqrt (0.9 * (avgpmax p))
    someCyclist = case viewl p of
      (c :< p') -> c

avgpmax :: (Seq Cyclist) -> Double
avgpmax p = (Fold.foldl (+) 0 (fmap pmax p)) / (fromIntegral $ Sequence.length p)

setSprinterSpeed :: Cyclist -> Cyclist
setSprinterSpeed c = c{speed = 1.76777 * spped * (tanh ((atanh (0.565685*(speed c) / spped)) + 0.538748*spped))}
  where spped = sqrt (0.95 * (pmax c))


pped :: (Cyclist, Char) -> Double
pped (c, 'p') = 0.8  * (pmax c)
pped (c, 'b') = 0.9  * (pmax c)
pped (c, 's') = 0.95 * (pmax c)


updateEnergy :: (Cyclist, Char) -> Cyclist
updateEnergy cc@(c, _) = c{usedEnergy = (usedEnergy c) + (60*((pped cc) - (pcp c)))} 
