module Simulation where

import Control.Monad.Random

import Cyclist
import Pack

end = 160000 :: Double -- 160 km

-- Don't quite get how to do the update here (unclear paper)
determineCoop :: Cyclist -> Rand StdGen Cyclist
determineCoop c = do
              d <- getRandom :: Rand StdGen Double
              return $ build (d < c_b c)
                            where
                                build b = Cyclist {max10 = max10 c, s_m = s_m c, e_rem = e_rem c, c_b = c_b c, c_t = c_t c, breakaway = breakaway c, speed = speed c, distance = distance c, position = position c, t_lead = t_lead c, team = team c, t_coop = t_coop c, b_coop = b}

-- Have to augment t_lead.
defLeader :: Pack -> Pack
defLeader (Pack (l:p))
  | t_lead l > 5 = Pack (p ++ [l]) -- OR is a defector
  | otherwise = Pack (l:p)

-- Update the speed, distance and effort of all riders in the pack.
update :: Pack -> Pack
update p = p

-- Don't know when/how I should handle breakaways.
turn :: Bool -> [Cyclist] -> Rand StdGen [Cyclist]
turn reCoop cs = cs' >>= (return . unpack . (map $ update . defLeader) . getPacks)
  where cs' = if reCoop then sequence (map determineCoop cs) else return cs
