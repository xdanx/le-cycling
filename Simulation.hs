module Simulation where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
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

-- Update position of Racers
updatePosition :: Race -> RandT StdGen IO Race
updatePosition (Race trn len packs sprint finish) = do
               let movedPacks = map updatePackPosition packs
                   movedSprinter = map (\c -> c{distance = (distance c) + 60*(speed c)}) sprint
                   (remainingPacks, toSprinters, packFinishers) = (\(a, b, c) -> (mapMaybe Prelude.id a, List.concat b, List.concat c)) . unzip3 . map (updatePack len) $ movedPacks
                   (sprintFinishers, remainingSprinters) = List.partition (\c -> (distance c) >= (fromIntegral len)) movedSprinter 
                   orderedFinishers = orderFinishers trn len $ sprintFinishers ++ packFinishers
                   newPackFuncs = coalescePacks $ remainingPacks
                   finalSprinters = map setSprinterSpeed (List.sort $ toSprinters ++ remainingSprinters)
               resetID
               newPacks <- sequence . map (\f -> newID >>= return . f) $ newPackFuncs
               return (Race trn len newPacks finalSprinters (finish ++ orderedFinishers))


-- Updates the position of a Pack (Pack or Breakaway): TESTED
updatePackPosition :: Pack -> Pack
updatePackPosition (Pack tLead leader pack uid) = let traveled = (speed leader)*60 in Pack tLead leader{distance = (distance leader) + traveled} (fmap (\c -> c{distance = (distance c) + traveled}) pack) uid
updatePackPosition (Breakaway pack time uid) = (Breakaway (fmap (\c -> c{distance = (distance c) + traveled}) pack) time uid)
                   where (h:<_) = viewl pack
                         traveled = 60 * speed h

--Splits pack into Pack, sprinters and finishers : TESTED
updatePack :: Int -> Pack -> (Maybe Pack, [Cyclist], [Cyclist])
updatePack len (Pack tLead leader pack puid) = if(t /= EmptyL)
           then (Nothing, Fold.toList sprinters, Fold.toList finishers)
           else if (Fold.or . fmap (\c -> (uid c) == (uid leader)) $ remainingPack) 
                   then ((Just $ Pack tLead leader (Sequence.filter (\c -> uid c /= uid leader) remainingPack) puid), Fold.toList sprinters, Fold.toList finishers)
                   else ((Just $ Pack tLead nleader nremainingPack puid), Fold.toList sprinters, Fold.toList finishers)
                         where allCyclists = leader <| pack
                               (finishers, runners) = Sequence.partition (\c -> (distance c) >= (fromIntegral len)) allCyclists
                               (sprinters, remainingPack) = Sequence.partition (\c -> (distance c) >= (fromIntegral $ len - 5000)) runners
                               t@(nleader:<nremainingPack) = viewl remainingPack

updatePack len (Breakaway pack time uid) = if(remainingPack == empty) 
           then (Nothing, Fold.toList sprinters, Fold.toList finishers)
           else ((Just $ Breakaway remainingPack time uid), Fold.toList sprinters, Fold.toList finishers)
           where (finishers, runners) = Sequence.partition (\c -> (distance c) >= (fromIntegral len)) pack
                 (sprinters, remainingPack) = Sequence.partition (\c -> (distance c) >= (fromIntegral $ len - 5000)) runners

coalescePacks :: [Pack] -> [Int -> Pack]
coalescePacks [] = []
coalescePacks packs = map (toFunc) . Prelude.foldl (\(l:ls) x -> if(overlap x l)
                                                             then (coalesce x l) ++ ls
                                                             else x:l:ls) [p] $ sp
                    where (p:sp) = List.sort $ packs
                          overlap :: Pack -> Pack -> Bool
                          overlap x y = ((Fold.foldl max 0 (fmap distance . getPack $ y)) + 3) >= (Fold.foldl min 0 (fmap distance . getPack $ x))
                          coalesce :: Pack -> Pack -> [Pack]
                          coalesce (Pack tLead1 leader1 pack1 uid1) (Pack _ leader2 pack2 _) = [Pack tLead1 leader1 (pack1 >< (leader2 <| pack2)) uid1]
                          coalesce (Pack tLead1 leader1 pack1 uid1) (Breakaway pack2 _ _) = [Pack tLead1 leader1 (pack1 >< pack2) uid1]
                          coalesce x@(Breakaway _ _ _) y@(Pack _ _ _ _) = coalesce y x
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
    update p = p

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
  brkPacks <- if((break >< break') == empty)
                 then return []
                 else mapM (\b -> newID >>= return . setPackSpeed . (Breakaway b 3)) . groupByTeam . (fmap fst) $ break >< break'
  let stayPack' = if seqElem stayPack l 
                  then [setPackSpeed (Pack tLead l (Sequence.filter ((uid l /=) . uid) stayPack) pid)] 
                  else case viewl stayPack of
                    EmptyL -> []
                    l' :< cs -> [setPackSpeed (Pack 0 l' cs pid)]
  return $ stayPack' ++ brkPacks
    where
      groupByTeam :: Seq Cyclist -> [Seq Cyclist]
      groupByTeam cs =
        case viewl cs of 
          EmptyL -> []
          c :< cs' -> brkPack : (groupByTeam cs'')
            where
              (brkPack, cs'') = Sequence.partition (\c' -> team c == team c') cs'

doBreakaway p = do return [p]
  
  
setPackSpeed :: Pack -> Pack
setPackSpeed pack@(Pack tLead l p pid) = packMap (\c -> c{speed = newSpeed}) pack
  where
    newSpeed = 1.76777 * ppped * (tanh ((atanh (0.565685*(speed l) / ppped)) + 0.538748*ppped))
    ppped = sqrt (0.8 * (avgpmax (l <| p)))
setPackSpeed pack@(Breakaway p t pid) = packMap (\c -> c{speed = newSpeed}) pack
  where 
    newSpeed = 1.76777 * bpped * (tanh ((atanh (0.565685*(speed someCyclist) / bpped)) + 0.538748*bpped))
    bpped = sqrt (0.9 * (avgpmax p))
    someCyclist = case viewl p of
      (c :< p') -> c

avgpmax :: (Seq Cyclist) -> Double
avgpmax p = (Fold.foldl (+) 0 (fmap pmax p)) / (fromIntegral $ Sequence.length p)

setSprinterSpeed :: Cyclist -> Cyclist
setSprinterSpeed c = c{speed = 1.76777 * spped * (tanh ((atanh (0.565685*(speed c) / spped)) + 0.538748*spped))}
  where spped = sqrt (0.95 * (pmax c))


pped :: (Cyclist, Char) -> Double
pped (c, 'p') = 0.8  * (pmax c)
pped (c, 'b') = 0.9  * (pmax c)
pped (c, 's') = 0.95 * (pmax c)


updateEnergy :: (Cyclist, Char) -> Cyclist
updateEnergy cc@(c, _) = c{usedEnergy = (usedEnergy c) + (60*((pped cc) - (pcp c)))} 


isBreak :: Pack -> Bool
isBreak (Pack {})      = False
isBreak (Breakaway {}) = True

determineCoop :: Cyclist -> RandT StdGen IO Cyclist
determineCoop c = do
              d1 <- getRandom :: RandT StdGen IO Double
              d2 <- getRandom :: RandT StdGen IO Double
              return $ c{genCoop = (d1 < genCProb c), teamCoop = (d2 < teamCProb c)}

defLeader :: Pack -> Pack
defLeader pack@(Pack tLead l p i)
  | (tLead > 5) || (not (genCoop l) && tLead > 1) = rotate pack 
  | otherwise = Pack (tLead+1) l p i
defLeader breakP = breakP


turn :: Race -> RandT StdGen IO Race
turn (Race trn len r s win) = do
     let reCompute = (trn `mod` 5 == 0)
     r' <- if reCompute
            then sequence (map (packMapM determineCoop) r) 
            else return r
     let   (Race _ _ r'' _ _) = updateBrkTime (Race trn len r' s win)
           r''' = map defLeader r''
     cyclists <- concatMapM doBreakaway r'''
     
     updatePosition $ (Race (trn + 1) len cyclists s win)

