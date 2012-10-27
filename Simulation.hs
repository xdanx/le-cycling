module Simulation where

import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Maybe

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

breakway :: Pack -> Rand StdGen (Pack, [Pack])
breakway (Pack p) = do
         dec <- replicateM (length p) (getRandom :: Rand StdGen Double)
         let (break', stay') = partition (\(c, d) -> (c_b c) < d) (zip p dec)
             groups = nub . map team . map fst $ break'
             (in_bteam, rest) = partition (flip elem groups . team) . map fst $ stay'
         g_dec <- replicateM (length in_bteam) (getRandom :: Rand StdGen Double)
         let (gbreak', gstay') = partition (\(c, d) -> (c_t c) < d) (zip in_bteam g_dec)
             stay = map fst $ stay' ++ gstay'
             breaks = map Pack . groupBy (\x y -> team x == team y) . map fst $ break' ++ gbreak'
         return (Pack stay, breaks)

set_pack_speed :: Pack -> Pack
set_pack_speed (Pack p) = Pack $ map (\c -> c{speed = speed}) p
               where speed = ((*0.8) . sum . map s_m $ p) / (fromIntegral . length $ p)
                

determineCoop :: Cyclist -> Rand StdGen Cyclist
determineCoop c = do
              d1 <- getRandom :: Rand StdGen Double
              d2 <- getRandom :: Rand StdGen Double
              return $ build (d1 < c_b c) (d2 < c_t c)
                            where
                                build b1 b2 = Cyclist {max10 = max10 c, s_m = s_m c, e_rem = e_rem c, c_b = c_b c, c_t = c_t c, breakaway = breakaway c, speed = speed c, distance = distance c, position = position c, t_lead = t_lead c, team = team c, t_coop = b2, b_coop = b1}

defLeader :: Pack -> Pack
defLeader (Pack (l:p))
  | (t_lead l > 5) || (not (b_coop l) && t_lead l > 1) = Pack (l'':p)
  | otherwise = Pack (l':p)
  where 
    l'  = l{t_lead = t_lead l + 1}
    l'' = l{t_lead = 0, distance = distance (last p)}


-- Update the speed, distance and effort of all riders in the pack.
update :: Pack -> Pack
update p = p

-- Don't know when/how I should handle breakaways.
turn :: Bool -> [Cyclist] -> Rand StdGen [Cyclist]
turn reCoop cs = cs' >>= (return . unpack . (map $ update . defLeader) . getPacks)
  where cs' = if reCoop then sequence (map determineCoop cs) else return cs
