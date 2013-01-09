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


import Cyclist
import Pack
import RungeKutta

-- Updates the speed of the cyclists in a pack
-- Creates new packs if some cyclists are too weak to follow the rest
setPackSpeed :: (MonadRandom m, MonadIO m) => Pack -> m [Pack]
setPackSpeed (Pack tLead l p uid) = do undefined
  where
    cyclists = sortBy (\x y -> compare (pmax x) (pmax y)) (l <| p)
    avgs = Sequence.zipWith (/) (Sequence.scanl1 (+) (fmap pmax cyclists)) (Sequence.fromList [1..])
    (drop, stay) = Sequence.partition (\(c,pm) -> (pmax c) < (0.8 * pm)) (Sequence.zip cyclists avgs)
    newCyclists = (fmap (\c -> updateCyclistSpeed c (0.8 * (pmax c))) (fmap fst stay))
    newPack = undefined
    dropPack = undefined
setPackSpeed (Breakaway p t uid) = undefined

-- Average pmax of a sequence of cyclists
avgpmax :: (Seq Cyclist) -> Double
avgpmax p = (Fold.foldl (+) 0 (fmap pmax p)) / (fromIntegral $ Sequence.length p)

-- Updates the speed of a single sprinter cyclist
setSprinterSpeed :: Cyclist -> Cyclist
setSprinterSpeed c = updateCyclistSpeed c (0.95 * (pmax c))

-- Update the used energy of a cyclists, depending on his speed
-- Need to do something different if it's in_pack vs (leader, breakaway or sprint)
updateEnergy :: Cyclist -> Cyclist
updateEnergy c = c{usedEnergy = (usedEnergy c) + 60 * ((pped c) - (pcp c))}
