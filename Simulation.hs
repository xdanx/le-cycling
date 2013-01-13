{- Simulation.hs
Functions that 
The function turn is called to simulate the state of the race
one minute after the given state.
-}

{-# LANGUAGE TupleSections, BangPatterns #-}
module Simulation where

import Control.Arrow
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Data.Foldable as Fold
import Data.List as List
import Data.Maybe
import Data.Sequence as Sequence

import Cyclist
import Modeling
import Pack
import Units
import Utils

-- Data type encoding the state of a race.
--                turns length  runners  sprinters   (finishers, finishTime)
data Race = Race  !Int  !Meters ![Pack]  ![Cyclist]  ![(Cyclist, Double)]
     deriving (Show)

-- Main function of the simulation. Do all the updates needed from
-- one state of the race to the next.
turn :: Race -> RandT StdGen IO Race
turn (Race trn len r s win) = do
     let
        s' = map updateEnergy s
        r1 = map (\p -> packMap updateEnergy p) r
        (Race _ _ r2 _ _) = updateBrkTime (Race trn len r1 s' win)
     r3 <- mapM defLeader r2
     cyclists <- concatMapM doBreakaway r3
     updatePosition $ (Race (trn + 1) len cyclists s' win)

--Updates the breakaway groups, i.e. reduce the timer then make it into a normal pack.
updateBrkTime :: Race -> Race
updateBrkTime (Race trn len r s w) = (Race trn len (map update r) s w)
  where
    update :: Pack -> Pack
    update (Breakaway p t i) = if(t > 0)
                               then (Breakaway p (t-1) i)
                               else case viewl p of
                                 l :< p' -> (Pack 0 l p' i)
    update p = p


-- Update position of Racers
updatePosition :: Race -> RandT StdGen IO Race --
updatePosition (Race trn len packs sprint finish) = do --
               let movedPacks = map updatePackPosition . List.filter (not . isEmpty) $ packs 
                   movedSprinter = map (\c -> c{distance = (distance c) + 60*(speed c)}) sprint
                   (remainingPacks, toSprinters, packFinishers) = (\(a, b, c) -> (mapMaybe id a, List.concat b, List.concat c)) . unzip3 . map (updatePack len) $ movedPacks --
                   (sprintFinishers, remainingSprinters) = List.partition (\c -> (distance c) >= len) movedSprinter 
                   orderedFinishers = orderFinishers trn len $ sprintFinishers ++ packFinishers
               newPacks' <- splitPacks remainingPacks
               let newPacks = coalescePacks newPacks'
                   finalSprinters = map setSprinterSpeed (List.sort $ toSprinters ++ remainingSprinters)
               return (Race trn len newPacks finalSprinters (finish ++ orderedFinishers))


-- Updates the position of a Pack (Pack or Breakaway): TESTED
updatePackPosition :: Pack -> Pack
updatePackPosition (Pack tLead leader pack uid) = let traveled = (speed leader)*60 in Pack tLead leader{distance = (distance leader) + traveled} (fmap (\c -> c{distance = (distance c) + traveled}) pack) uid
updatePackPosition (Breakaway pack time uid) = (Breakaway (fmap (\c -> c{distance = (distance c) + traveled}) pack) time uid)
                   where (h:<_) = viewl pack
                         traveled = 60 * speed h


--Splits pack into Pack, sprinters and finishers : TESTED
updatePack :: Meters -> Pack -> (Maybe Pack, [Cyclist], [Cyclist])
updatePack len (Pack tLead leader pack puid) = if(remainingPack == empty)
           then (Nothing, Fold.toList sprinters, Fold.toList finishers)
           else if (Fold.or . fmap (\c -> (uid c) == (uid leader)) $ remainingPack) 
                   then ((Just $ Pack tLead leader (Sequence.filter (\c -> uid c /= uid leader) remainingPack) puid), Fold.toList sprinters, Fold.toList finishers)
                   else ((Just $ Pack tLead nleader nremainingPack puid), Fold.toList sprinters, Fold.toList finishers)
                         where allCyclists = leader <| pack
                               (finishers, runners) = Sequence.partition (\c -> (distance c) >= len) allCyclists
                               (sprinters, remainingPack) = Sequence.partition (\c -> (distance c) >= (len - 5000)) runners
                               t@(nleader:<nremainingPack) = viewl remainingPack

updatePack len (Breakaway pack time uid) = if(remainingPack == empty) 
           then (Nothing, Fold.toList sprinters, Fold.toList finishers)
           else ((Just $ Breakaway remainingPack time uid), Fold.toList sprinters, Fold.toList finishers)
           where (finishers, runners) = Sequence.partition (\c -> (distance c) >= len) pack
                 (sprinters, remainingPack) = Sequence.partition (\c -> (distance c) >= (len - 5000)) runners

--Coalesce Packs that have collided
-- pre: input is sorted
coalescePacks :: [Pack] -> [Pack]
coalescePacks [] = []
coalescePacks packs = Prelude.foldl (\(l:ls) x -> if(overlap x l)
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
               
-- Split the packs that are broken.
splitPacks :: [Pack] -> RandT StdGen IO [Pack]
splitPacks packs = concatMapM splitPack packs

splitPack :: Pack -> RandT StdGen IO [Pack]
splitPack (Breakaway cs t _) = getPacks (Fold.toList cs) t
splitPack (Pack tLead l cs _) = do
  newPacks <- getPacks (Fold.toList $ l<|cs) 0 
  let (Pack _ l' cs' pid) = head newPacks
  return ((Pack tLead l' cs' pid):(tail newPacks))
  

--(fixedish) Make breakaways happen in a pack. Will output the rest of the original pack
-- and maybe new Breakaway packs.
doBreakaway :: Pack -> RandT StdGen IO [Pack] --
doBreakaway (Pack tLead l p pid) = do --
  (break, stay) <- partitionM packCoop (l <| p) --
  let brkTeams = fmap team break --
      (inBrkTeams, rest) = (Sequence.partition ((seqElem brkTeams) . team)) $ stay --
  (break', stay') <- partitionM teamCoop inBrkTeams  -- 
  let stayPack = stay' >< rest --
  brkPacks <- mapM (\b -> newID >>= return . setPackSpeed . (Breakaway b 3)) . groupByTeam $ break >< break' -- ~ (assume groupByTeam)
--  liftIO $ print stayPack
  let stayPack' = if seqElem stayPack l --
                  then [setPackSpeed (Pack tLead l (Sequence.filter ((uid l /=) . uid) stayPack) pid)]  --
                  else case viewl stayPack of
                    EmptyL -> [] --
                    l' :< cs -> [setPackSpeed (Pack 0 l' cs pid)] --
  return $ stayPack' ++ brkPacks  --
  where groupByTeam :: Seq Cyclist -> [Seq Cyclist]
        groupByTeam cs = case viewl cs of 
                              EmptyL -> []
                              c :< cs' -> (c <| brkPack) : (groupByTeam cs'')
                                where
                                        (brkPack, cs'') = Sequence.partition (\c' -> team c == team c') cs'

doBreakaway p = return [p]


--Takes the number of minutes already passed, the length of the race and a list of
-- cyclists and returns a list of pairs of cyclists and their respective finishing times : TESTED
orderFinishers :: Int -> Meters -> [Cyclist] -> [(Cyclist, Double)]
orderFinishers trn len = List.sortBy (\x y -> compare (snd x) (snd y)) . map (id &&& ((+fromIntegral(60*trn)) . pass)) 
               where pass :: Cyclist -> Double
                     pass c = (len - strt)/(speed c)
                          where
                                  strt = (distance c) - (fromIntegral 60) * (speed c)
