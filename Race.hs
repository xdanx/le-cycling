module Race where

import Data.List

import Cyclist
import Prelude

data Race = Race Int [Cyclist] [Cyclist]

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
