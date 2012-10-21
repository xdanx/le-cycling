module Simulation where

import Control.Monad.Random

import Cyclist
import Pack

end = 4000 :: Double

determineCoop :: Cyclist -> Cyclist
determineCoop c = c

defLeader :: Pack -> Pack
defLeader p = p 

turn :: Bool -> [Cyclist] -> Rand StdGen [Cyclist]
turn reCoop cs = return $ unpack . (map defLeader) . getPacks $ cs'
  where cs' = if reCoop then map determineCoop cs else cs
