module Pack where

import Data.Foldable as Fold
import Data.List as List
import Data.Sequence as Sequence

import Cyclist

--               tLead leader   pack           uid              pack           time uid
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
rotate :: Pack -> Pack
rotate (Pack t lead pack uid) = Pack 0 nLead nPack uid
       where seqDist = fmap distance (pack |> lead)
             (nPack:>nLead) = viewr $ Sequence.zipWith (\c d -> c{distance = d}) (lead <| pack) seqDist
        
packMap :: (Cyclist -> Cyclist) -> Pack -> Pack
packMap f (Pack tLead l p i) = (Pack tLead (f l) (fmap f p) i)
packMap f (Breakaway p t i) = (Breakaway (fmap f p) t i)

packMapM :: (Monad m) => (Cyclist -> m Cyclist) -> Pack -> m Pack
packMapM f (Pack tLead leader pack uid) = do
  leader' <- f leader
  pack' <- sequence . toList . fmap f $ pack
  return (Pack tLead leader' (fromList pack') uid)
packMapM f (Breakaway pack t i) = do
  pack' <- sequence . toList . fmap f $ pack
  return (Breakaway (fromList pack') t i)

packHead :: Pack -> Double
packHead (Pack _ leader _ _) = distance leader
packHead (Breakaway seq _ _) = Fold.foldl (max) 0 . fmap distance $ seq

getPack :: Pack -> Seq Cyclist
getPack (Pack _ leader pack _) = leader <| pack
getPack (Breakaway pack _ _) = pack

isEmpty :: Pack -> Bool
isEmpty (Breakaway cs _ _) = cs == empty
isEmpty _ = True

numCyclists :: Pack -> Int
numCyclists (Breakaway cs _ _) = Sequence.length cs
numCyclists (Pack _ _ cs _) = Sequence.length cs + 1

isBreak :: Pack -> Bool
isBreak (Pack {})      = False
isBreak (Breakaway {}) = True


-- Broken (halfway throught conversion), but probably useless.
-- unpack :: [Pack] -> [Cyclist] 
-- unpack [] = []
-- unpack ((Pack _ leader cs):ps) = (leader:()) ++ (unpack ps)
