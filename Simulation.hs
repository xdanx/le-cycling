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

import ID
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

-- updatePosition (Race trn len packs sprint finish) = (Race trn len )  
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
doBreakaway :: Pack -> RandT StdGen IO [Pack]
doBreakaway (Pack tLead l p pid) = do
  dec <- Sequence.replicateM ((Sequence.length p) + 1) (getRandom :: RandT StdGen IO Double)
  let (break, stay) = Sequence.partition (\(c, d) -> (genCProb c) < d) (Sequence.zip (l <| p) dec)  
      brkTeams = fmap team . fmap fst $ break
      (inBrkTeams, rest) = (Sequence.partition ((seqElem brkTeams) . team)) . (fmap fst) $ stay
  dec' <- Sequence.replicateM (Sequence.length inBrkTeams) (getRandom :: RandT StdGen IO Double)
  let (break', stay') = Sequence.partition (\(c, d) -> (genCProb c) < d) (Sequence.zip inBrkTeams dec')  
      stayPack = (fmap fst stay') >< rest
      brkPacks = map (setPackSpeed . (\b -> Breakaway b 3 newID)) . groupByTeam . (fmap fst) $ break >< break'
      stayPack' = if seqElem stayPack l 
                  then [setPackSpeed (Pack tLead l (Sequence.filter ((id l \=) . id) stayPack) pid)] 
                  else case viewl stayPack of
                    EmptyL -> []
                    l' :< cs -> [setPackSpeed (Pack 0 l' cs pid)]
  return stayPack' ++ brkPacks
    where
      groupByTeam :: Seq Cyclist -> [Seq Cyclist]
      groupByTeam cs =
        case viewl cs of 
          EmptyL -> []
          c :< cs' -> brkPack : (groupByTeam cs'')
            where
              (brkPack, cs'') = Sequence.partition (\c' -> team c == team c') cs'
  
  
setPackSpeed :: Pack -> Pack
setPackSpeed pack = 
  packMap (\c -> c{speed = nSpeed}) pack
  where 
    nSpeed = ((*perc) . (Fold.foldl (+) 0) $ (fmap speedM10 cs)) / (fromIntegral . Sequence.length $ cs)
    (cs, perc) = case pack of
      Pack _ l p _    -> ((l <| p), 0.8)
      Breakaway p _ _ -> (p,        0.9)

                        
updateSprintSpeed :: Cyclist -> Cyclist
updateSprintSpeed c
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
     cyclists <- concatMapM doBreakaway r'''
     return . updatePosition $ (Race (trn + 1) len cyclists s win)
