module Simulation where

import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Maybe

import Cyclist
import Pack
import Utils

data Race = Race Int [Cyclist] [Cyclist]

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

update_time :: Race -> Race
update_time (Race len r w) = flip (Race len) w . map (update) $ r
            where
                update :: Cyclist -> Cyclist
                update c = if(breakaway c > 0)
                                        then c{breakaway = (breakaway c) - 1}
                                        else c

do_breakaway :: Pack -> Rand StdGen [Pack]
do_breakaway (Pack p) = do
         dec <- replicateM (length p) (getRandom :: Rand StdGen Double)
         let (break', stay') = partition (\(c, d) -> (c_b c) < d) (zip p dec)
             groups = nub . map team . map fst $ break'
             (in_bteam, rest) = partition (flip elem groups . team) . map fst $ stay'
         g_dec <- replicateM (length in_bteam) (getRandom :: Rand StdGen Double)
         let (gbreak', gstay') = partition (\(c, d) -> (c_t c) < d) (zip in_bteam g_dec)
             stay = map fst $ stay' ++ gstay'
             breaks = map (set_pack_speed . Pack) . groupBy (\x y -> team x == team y) . map (\(c,_) -> c{breakaway = 3}) $ break' ++ gbreak'
         return ((set_pack_speed . Pack $ stay):breaks)

set_pack_speed :: Pack -> Pack
set_pack_speed pack@(Pack p) = Pack $ map (\c -> c{speed = speed}) p
               where speed = ((*perc) . sum . map s_m $ p) / (fromIntegral . length $ p)
                     perc = if(isBreak pack)
                                        then 0.9
                                        else 0.8
                
isBreak :: Pack -> Bool
isBreak (Pack p) =  and . map ((/=0) . breakaway) $ p

determineCoop :: Cyclist -> Rand StdGen Cyclist
determineCoop c = do
              d1 <- getRandom :: Rand StdGen Double
              d2 <- getRandom :: Rand StdGen Double
              return $ c{b_coop = (d1 < c_b c), t_coop = (d2 < c_t c)}

defLeader :: Pack -> Pack
defLeader (Pack (l:p))
  | (t_lead l > 5) || (not (b_coop l) && t_lead l > 1) = Pack (l'':p)
  | otherwise = Pack (l':p)
  where 
    l'  = l{t_lead = t_lead l + 1}
    l'' = l{t_lead = 0, distance = distance (last p)}

-- Update the speed, distance and effort of all riders in the pack.
{-update :: Pack -> Pack
update p = p-}

-- Don't know when/how I should handle breakaways.
turn :: Bool -> Race -> Rand StdGen Race
turn b r = do
     let t_r = update_time r
         (Race len p_r win) = update_position t_r 60
         packs = getPacks  p_r
     cyclist <- concatMapM do_breakaway packs
     return r


{-turn reCoop cs = cs' >>= (return . unpack . (map $ update . defLeader) . getPacks)
  where cs' = if reCoop then sequence (map determineCoop cs) else return cs-}

