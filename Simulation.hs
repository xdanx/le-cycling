module Simulation where

import Control.Monad.Random

import Cyclist
import Pack

end = 4000 :: Double

turn :: [Cyclist] -> Rand StdGen [Cyclist]
turn = return
