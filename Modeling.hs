module Modeling where

import Data.Foldable as Fold
import Data.Sequence as Sequence

import Cyclist
import Pack
import RungeKutta

setPackSpeed :: Pack -> [Pack]
setPackSpeed p = undefined
  where
    coef = if isBreak p then 0.9 else 0.8
    cyclists = case p of
      Pack _ l p' _ -> sortBy (\x y -> compare (pmax x) (pmax y)) (l <| p')
      Breakaway p' _ _ -> sortBy (\x y -> compare (pmax x) (pmax y)) p'
    avgs = Sequence.zipWith (/) (Sequence.scanl1 (+) (fmap pmax cyclists)) (fromList [1..])
    (drop, stay) = Sequence.partition (\(c,pm) -> (pmax c) < (coef * pm)) (Sequence.zip cyclists avgs)
    newCyclists = (fmap (\c -> updateCyclistSpeed c (coef * (pmax c))) (fmap fst stay))
    newPack = case p of
      Breakaway _ t i -> Breakaway newCyclists t i
      Pack t _ _ i    -> undefined

avgpmax :: (Seq Cyclist) -> Double
avgpmax p = (Fold.foldl (+) 0 (fmap pmax p)) / (fromIntegral $ Sequence.length p)

setSprinterSpeed :: Cyclist -> Cyclist
setSprinterSpeed c = updateCyclistSpeed c (0.95 * (pmax c))

-- Need to do something different if it's in_pack vs (leader, breakaway or sprint)
updateEnergy :: Cyclist -> Cyclist
updateEnergy c = undefined
