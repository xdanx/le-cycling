module Simulation where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Random
import Data.List as List
import Data.Sequence as Sequence
import Data.Foldable as Fold
import Data.Maybe
import Debug.Trace
import Control.Exception (assert)

import Cyclist
import Pack
import Utils

--                length  turns  runners  sprinters   (finishers, finishTime)
data Race = Race  !Int    !Int   ![Pack]  ![Cyclist]  ![(Cyclist, Double)]
     deriving (Show)

-- !!! NEED TO FINISH UPDATING !!!
-- Update position of Racers
updatePosition :: Race -> Race
updatePosition (Race trn len packs sprint finish) = undefined
  
  
{-  (Race trn len packs' sprint'' (finish ++ sfinishers'))
  where
    packs' = (map (\(Pack tl l cs i) -> ((updPos l) <| (fmap updPos cs))) packs)::[Seq Cyclist]
    
    sprintLim = (\c -> (fromIntegral (len - 5000)) <= (distance c))
    
    (sprint', packs'') = Sequence.partition sprintLim packs'
    
    updateSprint = map updPos sprint
    (finishers, sprint'') = List.partition (\c -> (fromIntegral len) <= (distance c)) 
                            (updateSprint ++ (map update_sprint_speed sprint'))
    
    sfinishers = List.sortBy (\x y -> compare (pass x) (pass y)) finishers
    sfinishers' = map (\c -> (c, (fromIntegral (trn*60)) + (pass c))) sfinishers
    
    updPos :: Cyclist -> Cyclist
    updPos c = c{distance = (distance c) + (fromIntegral 60) * (speed c)}
    
    pass :: Cyclist -> Double
    pass c = ((fromIntegral len) - strt)/(speed c)
      where
        strt = (distance c) - (fromIntegral 60) * (speed c)-}


-- !!! How should we generate new unique IDs and do we have to do it here ? !!!
updateBrkTime :: Race -> Race
updateBrkTime (Race trn len r s w) = (Race trn len (map update r) s w)
                where
                     update :: Pack -> Pack
                     update (Breakaway p t i) = if(t > 0)
                                        then (Breakaway p (t-1) i)
                                        else case viewl p of
                                                  l :< p' -> (Pack 0 l p' i)

-- !!! Just removed name conflicts !!!
do_breakaway :: Pack -> RandT StdGen IO [Pack]
do_breakaway (Pack _ _ p _) = do
         dec <- Control.Monad.replicateM (Sequence.length (p + 1)) (getRandom :: RandT StdGen IO Double)
         let (break', stay') = List.partition (\(c, d) -> (genCProb c) < d) (List.zip p dec)
             groups = nub . map team . map fst $ break'
             (in_bteam, rest) = List.partition (flip List.elem groups . team) . map fst $ stay'
         g_dec <- Control.Monad.replicateM (List.length in_bteam) (getRandom :: RandT StdGen IO Double)
         let (gbreak', gstay') = List.partition (\(c, d) -> (teamCProb c) < d) (List.zip in_bteam g_dec)
             stay =  (map fst gstay') ++ rest
             breaks = map (set_pack_speed . Pack) . groupBy (\x y -> team x == team y) . map (\(c,_) -> c{breakaway = 3}) $ break' ++ gbreak'
         return ((set_pack_speed . Pack $ stay):breaks)

set_pack_speed :: Pack -> Pack
set_pack_speed pack = 
  packMap (\c -> c{speed = nSpeed}) pack
  where 
    nSpeed = ((*perc) . (Fold.foldl (+) 0) $ (fmap speedM10 cs)) / (fromIntegral . Sequence.length $ cs)
    (cs, perc) = case pack of
      Pack _ l p _    -> ((l <| p), 0.8)
      Breakaway p _ _ -> (p,        0.9)

                        
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
isBreak (Pack {})      = False
isBreak (Breakaway {}) = True

determineCoop :: Cyclist -> RandT StdGen IO Cyclist
determineCoop c = do
              d1 <- getRandom :: RandT StdGen IO Double
              d2 <- getRandom :: RandT StdGen IO Double
              return $ c{genCoop = (d1 < genCProb c), teamCoop = (d2 < teamCProb c)}

defLeader :: Pack -> Pack
defLeader (Pack tLead l p id)
  | (tLead > 5) || (not (genCoop l) && tLead > 1) = Pack (tLead+1) l p id
  | otherwise = Pack 0 l' (p |> l) id
  where
    l' = case (viewl p) of
      EmptyL -> l
      c :< cs -> c 
defLeader breakP = breakP


turn :: Race -> RandT StdGen IO Race
turn (Race trn len r s win) = do
     let reCompute = (trn `mod` 5 == 0)
     r' <- if reCompute
            then sequence (map (packMap determineCoop) r) 
            else return r
     let   (Race _ _ r'' _ _) = updateBrkTime (Race trn len r' s win)
           r''' = map defLeader r''
     cyclists <- concatMapM do_breakaway r'''
     return . updatePosition $ (Race (trn + 1) len cyclists s win)
