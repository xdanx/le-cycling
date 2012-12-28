module Modeling where

import Data.Foldable as Fold
import Data.Sequence as Sequence

import Cyclist
import Pack
import RungeKutta

setPackSpeed :: Pack -> [Pack]
setPackSpeed p = undefined

avgpmax :: (Seq Cyclist) -> Double
avgpmax p = (Fold.foldl (+) 0 (fmap pmax p)) / (fromIntegral $ Sequence.length p)

setSprinterSpeed :: Cyclist -> Cyclist
setSprinterSpeed c = updateCyclistSpeed c (pped (c, 's'))

pped :: (Cyclist, Char) -> Double
pped (c, 'p') = 0.8  * (pmax c)
pped (c, 'b') = 0.9  * (pmax c)
pped (c, 's') = 0.95 * (pmax c)

updateEnergy :: (Cyclist, Char) -> Cyclist
updateEnergy cc@(c, _) = c{usedEnergy = (usedEnergy c) + (60*((pped cc) - (pcp c)))} 
