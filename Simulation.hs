module Simulation where

import Control.Monad.Random

import Cyclist
import Pack

end = 4000 :: Double

-- Don't quite get how to do the update here (unclear paper)
determineCoop :: Cyclist -> Cyclist
determineCoop c = c

-- Have to keep the time a cyclist is staying at 
-- the head of the pack, problem : this has to be passed 
-- at each turn, so either redesign so main loop to use [Pack]
-- or keep that info in the Cyclist.
defLeader :: Pack -> Pack
defLeader p = p 

-- Update the speed, distance and effort of all riders in the pack.
update :: Pack -> Pack
update p = p

-- Don't know when/how I should handle breakaways.
turn :: Bool -> [Cyclist] -> Rand StdGen [Cyclist]
turn reCoop cs = return $ unpack . (map $ update . defLeader) . getPacks $ cs'
  where cs' = if reCoop then map determineCoop cs else cs
