module Simulation where

import Control.Monad.Random
import Data.List

import Cyclist
import Pack

data Race = Race Int [Cyclist] [Cyclist]

-- end = 160000 :: Double -- 160 km

-- Update position of Racers
update_position :: Race -> Int -> Race
update_position (Race len race finish) time = (Race len (sort racers) (finish ++ sfinishers))
                where 
                      update = map (\c -> c{distance = (distance c) + (fromIntegral time) * (speed c)}) race
                      (finishers, racers) = partition (\c -> (fromIntegral len) <= (distance c)) update
                      sfinishers = sortBy (\x y -> compare (pass x) (pass y)) finishers
                      pass :: Cyclist -> Double
                      pass c = ((fromIntegral len) - strt)/(speed c)
                           where
                                strt = (distance c) - (fromIntegral time) * (speed c)

-- Don't quite get how to do the update here (unclear paper)
determineCoop :: Cyclist -> Rand StdGen Cyclist
determineCoop c = do
              d1 <- getRandom :: Rand StdGen Double
              d2 <- getRandom :: Rand StdGen Double
              return $ build (d1 < c_b c) (d2 < c_t c)
                            where
                                build b1 b2 = Cyclist {max10 = max10 c, s_m = s_m c, e_rem = e_rem c, c_b = c_b c, c_t = c_t c, breakaway = breakaway c, speed = speed c, distance = distance c, position = position c, t_lead = t_lead c, team = team c, t_coop = b2, b_coop = b1}

-- Have to augment t_lead.
defLeader :: Pack -> Pack
defLeader (Pack (l:p))
  | (t_lead l > 5) || (not (b_coop l) && t_lead l > 1) = Pack (p ++ [l''])
  | otherwise = Pack (l':p)
  where 
    l'  = l{t_lead = t_lead l + 1}
    l'' = l{t_lead = 0}


-- Update the speed, distance and effort of all riders in the pack.
update :: Pack -> Pack
update p = p

-- Don't know when/how I should handle breakaways.
turn :: Bool -> [Cyclist] -> Rand StdGen [Cyclist]
turn reCoop cs = cs' >>= (return . unpack . (map $ update . defLeader) . getPacks)
  where cs' = if reCoop then sequence (map determineCoop cs) else return cs
