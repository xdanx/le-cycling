module Simulation where

import Control.Monad.Random

import Cyclist
import Pack

end = 160000 :: Double -- 160 km

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
  | (t_lead l > 5) || (not (b_coop l) && t_lead l > 1) = Pack (p ++ [l])
  | otherwise = Pack (l':p)
  where 
    l' = Cyclist {max10 = max10 l, s_m = s_m l, e_rem = e_rem l, c_b = c_b l, c_t = c_t l, breakaway = breakaway l, speed = speed l, distance = distance l, position = position l, t_lead = (t_lead l) + 1, team = team l, t_coop = t_coop l, b_coop = b_coop l}

-- Update the speed, distance and effort of all riders in the pack.
update :: Pack -> Pack
update p = p

-- Don't know when/how I should handle breakaways.
turn :: Bool -> [Cyclist] -> Rand StdGen [Cyclist]
turn reCoop cs = cs' >>= (return . unpack . (map $ update . defLeader) . getPacks)
  where cs' = if reCoop then sequence (map determineCoop cs) else return cs
