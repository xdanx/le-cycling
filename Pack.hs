{- Pack.hs
Define the type Pack which represents an indivudual pack.
It could be a normal pack or a breakaway pack.
Normal packs contain a leader and how long he has been leading.
Breakaway packs contain the time left before it becomes a normal pack.
-}

module Pack where

import Control.Monad.Random hiding (fromList)
import Data.Foldable as Fold
import Data.List as List
import Data.Sequence as Sequence

import Cyclist
import Units

--               tLead leader   cyclists       uid  |           cyclists       time uid
data Pack = Pack !Int  !Cyclist !(Seq Cyclist) !Int | Breakaway !(Seq Cyclist) !Int !Int
            deriving(Show)

instance Eq Pack where
         a == b = (packHead a == packHead b)
         a /= b = not (a == b)

instance Ord Pack where
         compare a b = compare (packHead a) (packHead b)
         a < b = (packHead a) < (packHead b)
         a <= b = (packHead a) <= (packHead b)
         a >= b = (packHead a) >= (packHead b)
         a > b = (packHead a) > (packHead b)
         max a b
             | a < b = b
             | otherwise = a
         min a b
             | a < b = a
             |otherwise = b


{-instance Show Pack where
         show (Pack l) = show $ map (distance) l1-}

-- If the leader decides to act as a defector or he has been
-- leading for 5 turns, we rotate the pack (get a new leader)
defLeader :: Pack -> RandT StdGen IO Pack
defLeader pack@(Pack tLead l p i) = do
                                  leaderCoops <- packCoop l
                                  if tLead > 5
                                       then return . rotate $ pack
                                       else if leaderCoops
                                               then return $ Pack (tLead + 1) l p i
                                               else return . rotate $ pack
defLeader breakP = return breakP -- no leaders in breakaway packs.


--Pack Utils:

-- Form packs from a list of Cyclists. 
-- A pack is defined by the maximum set of cyclists such that any cyclist
-- in the pack is at less than 3 meters from another cyclist in the pack
getPacks :: [Cyclist] -> [Pack]
getPacks cyclists =
  List.zipWith ($) (Prelude.foldl addToPackList [] (List.sort cyclists)) [1..]
  where
    addToPackList :: [Int -> Pack] -> Cyclist -> [Int -> Pack]
    addToPackList [] c =
      [Pack 0 c empty ]
    addToPackList (h:ps) c
      | ((distance c) - (distance leader) < 3) = (Pack tLead c (leader <| cs)):ps
      | otherwise = (Pack 0 c empty):(Pack tLead leader cs):ps
                    where (Pack tLead leader cs _) = h 0
        
-- rotates leader : TESTED
-- Puts the leader at the back of the pack and chooses the next leader.
rotate :: Pack -> Pack
rotate (Pack t lead pack uid) = Pack 0 nLead nPack uid
       where seqDist = fmap distance (pack |> lead)
             (nPack:>nLead) = viewr $ Sequence.zipWith (\c d -> c{distance = d}) (lead <| pack) seqDist
        
-- Maps a function over all the cyclists of a pack.
packMap :: (Cyclist -> Cyclist) -> Pack -> Pack
packMap f (Pack tLead l p i) = (Pack tLead (f l) (fmap f p) i)
packMap f (Breakaway p t i) = (Breakaway (fmap f p) t i)

-- is to packMap what mapM is to map.
packMapM :: (Monad m) => (Cyclist -> m Cyclist) -> Pack -> m Pack
packMapM f (Pack tLead leader pack uid) = do
  leader' <- f leader
  pack' <- sequence . toList . fmap f $ pack
  return (Pack tLead leader' (fromList pack') uid)
packMapM f (Breakaway pack t i) = do
  pack' <- sequence . toList . fmap f $ pack
  return (Breakaway (fromList pack') t i)

-- Gets the distance at the top of the pack.
packHead :: Pack -> Meters
packHead (Pack _ leader _ _) = distance leader
packHead (Breakaway seq _ _) = Fold.foldl (max) 0 . fmap distance $ seq

-- Gets the sequence of all the cyclists in the pack.
getPack :: Pack -> Seq Cyclist
getPack (Pack _ leader pack _) = leader <| pack
getPack (Breakaway pack _ _) = pack

-- Test if a pack as no cyclists in it.
isEmpty :: Pack -> Bool
isEmpty (Breakaway cs _ _) = cs == empty
isEmpty _ = True -- <--- Why do we return True here ?

-- Number of cyclists contained in a pack.
numCyclists :: Pack -> Int
numCyclists (Breakaway cs _ _) = Sequence.length cs
numCyclists (Pack _ _ cs _) = Sequence.length cs + 1

-- Is it a breakaway pack ?
isBreak :: Pack -> Bool
isBreak (Pack {}) = False
isBreak (Breakaway {}) = True
