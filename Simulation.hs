module Simulation where

import Control.Arrow
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
import ID
import Pack
import Utils

--                length  turns  runners  sprinters   (finishers, finishTime)
data Race = Race  !Int    !Int   ![Pack]  ![Cyclist]  ![(Cyclist, Double)]
     deriving (Show)

-- !!! NEED TO FINISH UPDATING !!!
-- Update position of Racers
updatePosition :: Race -> RandT StdGen IO Race
updatePosition (Race trn len packs sprint finish) = do
               let movedPacks = map updatePackPosition packs
                   movedSprinter = map (\c -> c{distance = (distance c) + 60*(speed c)}) sprint
                   (remainingPacks, toSprinters, packFinishers) = (\(a, b, c) -> (a, List.concat b, List.concat c)) . unzip3 . map (updatePack len) $ movedPacks
                   (sprintFinishers, remainingSprinters) = List.partition (\c -> (distance c) >= (fromIntegral len)) movedSprinter 
                   orderedFinishers = orderFinishers trn len $ sprintFinishers ++ packFinishers
                   newPackFuncs = coalescePacks $ remainingPacks
               resetID
               newPacks <- sequence . map (\f -> newID >>= return . f) $ newPackFuncs
               return (Race trn len newPacks (List.sort $ toSprinters ++ remainingSprinters) (finish ++ orderedFinishers))


-- Updates the position of a Pack (Pack or Breakaway): TESTED
updatePackPosition :: Pack -> Pack
updatePackPosition (Pack tLead leader pack uid) = let traveled = (speed leader)*60 in Pack tLead leader{distance = (distance leader) + traveled} (fmap (\c -> c{distance = (distance c) + traveled}) pack) uid
updatePackPosition (Breakaway pack time uid) = (Breakaway (fmap (\c -> c{distance = (distance c) + traveled}) pack) time uid)
                   where (h:<r) = viewl pack
                         traveled = 60 * speed h

--Splits pack into Pack, sprinters and finishers : TESTED
updatePack :: Int -> Pack -> (Pack, [Cyclist], [Cyclist])
updatePack len (Pack tLead leader pack uid) = if (Fold.foldl (||) False . fmap (\c -> (Cyclist.id c) == (Cyclist.id leader)) $ remainingPack) 
                 then (Pack tLead leader remainingPack uid, Fold.toList sprinters, Fold.toList finishers)
                 else (Pack tLead nleader nremainingPack uid, Fold.toList sprinters, Fold.toList finishers)
           where allCyclists = leader <| pack
                 (finishers, runners) = Sequence.partition (\c -> (distance c) >= (fromIntegral len)) allCyclists
                 (sprinters, remainingPack) = Sequence.partition (\c -> (distance c) >= (fromIntegral $ len - 5000)) runners
                 (nleader:<nremainingPack) = viewl remainingPack

coalescePacks :: [Pack] -> [Int -> Pack]
coalescePacks [] = []
coalescePacks packs = map (toFunc) . Prelude.foldl (\(l:ls) x -> if(overlap x l)
                                                             then (coalesce x l) ++ ls
                                                             else x:l:ls) [p] $ sp
                    where (p:sp) = List.sort $ packs
                          overlap :: Pack -> Pack -> Bool
                          overlap x y = ((Fold.foldl max 0 (fmap distance . getPack $ y)) + 3) >= (Fold.foldl min 0 (fmap distance . getPack $ x))
                          coalesce :: Pack -> Pack -> [Pack]
                          coalesce (Pack tLead1 leader1 pack1 uid1) (Pack tLead2 leader2 pack2 uid2) = [Pack tLead1 leader1 (pack1 >< (leader2 <| pack2)) uid1]
                          coalesce (Pack tLead1 leader1 pack1 uid1) (Breakaway pack2 time2 uid2) = [Pack tLead1 leader1 (pack1 >< pack2) uid1]
                          coalesce x@(Breakaway pack2 time2 uid2) y@(Pack tLead1 leader1 pack1 uid1) = coalesce y x
                          coalesce x@(Breakaway pack1 time1 uid1) y@(Breakaway pack2 time2 uid2) = 
                                   if(team1 == team2)
                                            then [Breakaway (pack1 >< pack2) (min time1 time2) uid1]
                                            else [x, y]
                                            where 
                                                  (h1:<_) = viewl pack1
                                                  (h2:<_) = viewl pack2
                                                  team1 = team h1
                                                  team2 = team h2
                          toFunc :: Pack -> (Int -> Pack)
                          toFunc (Pack tLead leader pack _) = Pack tLead leader pack
                          toFunc (Breakaway pack time uid) = Breakaway pack time
                          
--Takes the number of minutes already passed, the length of the race and a list of
-- cyclists and returns a list of pairs of cyclists and their respective finishing times : TESTED
orderFinishers :: Int -> Int -> [Cyclist] -> [(Cyclist, Double)]
orderFinishers trn len = List.sortBy (\x y -> compare (snd x) (snd y)) . map (Prelude.id &&& ((+fromIntegral(60*trn)) . pass)) 
               where pass :: Cyclist -> Double
                     pass c = ((fromIntegral len) - strt)/(speed c)
                          where
                                  strt = (distance c) - (fromIntegral 60) * (speed c)

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
                  then [setPackSpeed (Pack tLead l (Sequence.filter ((Cyclist.id l /=) . Cyclist.id) stayPack) pid)] 
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
     updatePosition $ (Race (trn + 1) len cyclists s win)

