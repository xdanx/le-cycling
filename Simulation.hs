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
import ID
import Modeling
import Pack
import Utils
import Units

--                turns length  runners  sprinters   (finishers, finishTime)
data Race = Race  !Int  !Meters ![Pack]  ![Cyclist]  ![(Cyclist, Double)]
     deriving (Show)


turn :: Race -> RandT StdGen IO Race
turn (Race trn len r s win) = do
     let
        s' = map updateEnergy s
        r' = map (\p -> packMap updateEnergy p) r
        (Race _ _ r'' _ _) = updateBrkTime (Race trn len r' s' win)
     r''' <- mapM defLeader r''
     cyclists <- concatMapM doBreakaway r'''
     let before = List.sum . map numCyclists $ r'''
         after = List.sum . map numCyclists $ cyclists
     when (before /= after) . liftIO . putStrLn $ "You being foolish again fool, before: " ++ show before ++ ", after: " ++ show after ++ (if before < 5 then show r''' ++ ", " ++ show cyclists else "")
     updatePosition $ (Race (trn + 1) len cyclists s' win)

--Updates the breakaway groups
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
updatePosition :: Race -> RandT StdGen IO Race
updatePosition (Race trn len packs sprint finish) = do
               let movedPacks = map updatePackPosition . List.filter (not . isEmpty) $ packs
                   movedSprinter = map (\c -> c{distance = (distance c) + 60*(speed c)}) sprint
                   (remainingPacks, toSprinters, packFinishers) = (\(a, b, c) -> (mapMaybe id a, List.concat b, List.concat c)) . unzip3 . map (updatePack len) $ movedPacks
                   (sprintFinishers, remainingSprinters) = List.partition (\c -> (distance c) >= len) movedSprinter 
                   orderedFinishers = orderFinishers trn len $ sprintFinishers ++ packFinishers
                   newPacks = coalescePacks $ remainingPacks
                   finalSprinters = map setSprinterSpeed (List.sort $ toSprinters ++ remainingSprinters)
               when (List.or . map isEmpty $ newPacks) . liftIO . putStrLn $ "The fuck you playing at fool?"
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
               

--fixedish 
doBreakaway :: Pack -> RandT StdGen IO [Pack]
doBreakaway (Pack tLead l p pid) = do
  (break, stay) <- partitionM packCoop (l <| p) 
  let brkTeams = fmap team break
      (inBrkTeams, rest) = (Sequence.partition ((seqElem brkTeams) . team)) $ stay
  (break', stay') <- partitionM teamCoop inBrkTeams  
  let stayPack = stay' >< rest
  brkPacks <- concatMapM (\b -> newID >>= setPackSpeed . (Breakaway b 3)) . groupByTeam $ break >< break'
  stayPack' <- if seqElem stayPack l 
               then setPackSpeed (Pack tLead l (Sequence.filter ((uid l /=) . uid) stayPack) pid)
                  else case viewl stayPack of
                            EmptyL -> return []
                            l' :< cs -> setPackSpeed (Pack 0 l' cs pid)
  return $ stayPack' ++ brkPacks
    where
      groupByTeam :: Seq Cyclist -> [Seq Cyclist]
      groupByTeam cs =
        case viewl cs of 
          EmptyL -> []
          c :< cs' -> brkPack : (groupByTeam cs'')
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
