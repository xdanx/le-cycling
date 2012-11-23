module Simulation where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Random
import Data.List
import Data.Maybe
import Debug.Trace
import Control.Exception (assert)

import Cyclist
import Pack
import Utils

-- length turns runners sprinters (finishers, time)
data Race = Race !Int !Int ![Cyclist] ![Cyclist] ![(Cyclist, Double)]
     deriving (Show)

-- Update position of Racers
update_position :: Race -> Race
update_position (Race trn len race sprint finish) = (Race trn len (sort racers) sprint'' (finish ++ sfinishers'))
                where
                      up_pos = (\c -> c{distance = (distance c) + (fromIntegral time) * (speed c)})
                      time = 60
                      update = map up_pos race
                      (sprint', racers) = partition (\c -> (fromIntegral (len - 5000)) <= (distance c)) update
                      updateSprint = map up_pos sprint
                      (finishers, sprint'') = partition (\c -> (fromIntegral len) <= (distance c)) (updateSprint ++ (map update_sprint_speed sprint'))
                      sfinishers = sortBy (\x y -> compare (pass x) (pass y)) finishers
                      sfinishers' = map (\c -> (c, (fromIntegral (trn*60)) + (pass c))) sfinishers
                      pass :: Cyclist -> Double
                      pass c = ((fromIntegral len) - strt)/(speed c)
                           where
                                strt = (distance c) - (fromIntegral time) * (speed c)

update_time :: Race -> Race
update_time (Race trn len r s w) = (Race trn len (map update r) s w)
                where
                     update :: Cyclist -> Cyclist
                     update c = if(breakaway c > 0)
                                        then c{breakaway = (breakaway c) - 1}
                                        else c

do_breakaway :: Pack -> RandT StdGen IO [Pack]
do_breakaway (Pack p) = do
         dec <- replicateM (length p) (getRandom :: RandT StdGen IO Double)
         let (break', stay') = partition (\(c, d) -> (genCProb c) < d) (zip p dec)
             groups = nub . map team . map fst $ break'
             (in_bteam, rest) = partition (flip elem groups . team) . map fst $ stay'
         g_dec <- replicateM (length in_bteam) (getRandom :: RandT StdGen IO Double)
         let (gbreak', gstay') = partition (\(c, d) -> (teamCProb c) < d) (zip in_bteam g_dec)
             stay =  (map fst gstay') ++ rest
             breaks = map (set_pack_speed . Pack) . groupBy (\x y -> team x == team y) . map (\(c,_) -> c{breakaway = 3}) $ break' ++ gbreak'
         return ((set_pack_speed . Pack $ stay):breaks)

set_pack_speed :: Pack -> Pack
set_pack_speed pack@(Pack p) = Pack $ map (\c -> c{speed = speed}) p
               where speed = ((*perc) . sum . map speedM10 $ p) / (fromIntegral . length $ p)
                     perc = if(isBreak pack)
                                        then 0.9
                                        else 0.8

update_sprint_speed :: Cyclist -> Cyclist
update_sprint_speed c
              | t > 30 = c{speed = 0.9*(speedM10 c)}
              | t < 1 = c{speed = 0.5 * (speedM10 c)}
              | otherwise = c{speed = 0.7*(speedM10 c)}
                    where t = 60 * tlim c

tlim :: Cyclist -> Double
tlim c = exp (-6.35 * ((ptot c)/(max10 c)) + 2.478)
     where ptot c = pair c + proll c
                   where
                        pair c = (speed c)^3
                        proll c = 9.8*(cweight + bweight) * (speed c)
                        cweight = 60
                        bweight = 5

isBreak :: Pack -> Bool
isBreak (Pack p) =  and . map ((/=0) . breakaway) $ p

determineCoop :: Cyclist -> RandT StdGen IO Cyclist
determineCoop c = do
              d1 <- getRandom :: RandT StdGen IO Double
              d2 <- getRandom :: RandT StdGen IO Double
              return $ c{genCoop = (d1 < genCProb c), teamCoop = (d2 < teamCProb c)}

defLeader :: Pack -> Pack
defLeader (Pack (l:p))
  | (tLead l > 5) || (not (genCoop l) && tLead l > 1) = Pack (l'':p)
  | otherwise = Pack (l':p)
  where
    l'  = l{tLead = tLead l + 1}
    l'' = l{tLead = 0, distance = (distance (last (l:p))) - 1}

-- Don't know when/how I should handle breakaways.
turn :: Race -> RandT StdGen IO Race
turn (Race trn len r s win) = do
     let b = (trn `mod` 5 == 0)
     c_r <- if b then sequence (map determineCoop r) else return r
     let   (Race _ _ t_r _ _) = update_time (Race trn len c_r s win)
           packs = getPacks t_r
           l_p = map (\p -> if(isBreak $ p) then p else defLeader p) packs
     cyclist <- concatMapM do_breakaway l_p
     return . update_position $ (Race (trn + 1) len (unpack cyclist) s win)
