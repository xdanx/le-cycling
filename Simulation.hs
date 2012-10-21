module Simulation where

import Control.Monad.Random

import Cyclist
import Pack

end = 4000 :: Double

determineCoop :: Cyclist -> Cyclist
determineCoop c = c

turn :: Bool -> [Cyclist] -> Rand StdGen [Cyclist]
turn reCoop cs = return $ unpack . getPacks $ cs'
  where cs' = if reCoop then map determineCoop cs else cs
