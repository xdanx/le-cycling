module Simulation where

import Control.Monad.Random

import Cyclist
import Pack

end = 4000 :: Double

-- Don't quite get how to do the update here (unclear paper)
determineCoop :: Cyclist -> Cyclist
determineCoop c = c

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
turn reCoop cs = return $ unpack . (map $ update . defLeader) . getPacks $ cs'
  where cs' = if reCoop then map determineCoop cs else cs
