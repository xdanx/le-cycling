module Modeling where

import Data.Foldable as Fold
import Data.Sequence as Sequence

import Cyclist
import Pack
import RungeKutta

-- Need some monadic shit for new pack ids
setPackSpeed :: Pack -> [Pack]
setPackSpeed (Pack tLead l p uid) = undefined
  where
    cyclists = sortBy (\x y -> compare (pmax x) (pmax y)) (l <| p)
    avgs = Sequence.zipWith (/) (Sequence.scanl1 (+) (fmap pmax cyclists)) (fromList [1..])
    (drop, stay) = Sequence.partition (\(c,pm) -> (pmax c) < (0.8 * pm)) (Sequence.zip cyclists avgs)
    newCyclists = (fmap (\c -> updateCyclistSpeed c (0.8 * (pmax c))) (fmap fst stay))
    newPack = undefined
setPackSpeed (Breakaway p t uid) = undefined

avgpmax :: (Seq Cyclist) -> Double
avgpmax p = (Fold.foldl (+) 0 (fmap pmax p)) / (fromIntegral $ Sequence.length p)

setSprinterSpeed :: Cyclist -> Cyclist
setSprinterSpeed c = updateCyclistSpeed c (0.95 * (pmax c))

-- Need to do something different if it's in_pack vs (leader, breakaway or sprint)
updateEnergy :: Cyclist -> Cyclist
updateEnergy c = undefined
