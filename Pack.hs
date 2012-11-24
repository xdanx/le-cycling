module Pack where

import Cyclist
import Data.List as List
import Data.Sequence as Sequence

--               tLead leader   pack           uid              pack           uid
data Pack = Pack !Int  !Cyclist !(Seq Cyclist) !Int | Breakaway !(Seq Cyclist) !Int
            deriving(Show)

{-instance Show Pack where
         show (Pack l) = show $ map (distance) l1-}

getPacks :: [Cyclist] -> [Pack]
getPacks cyclists =
  List.zipWith ($) (foldl addToPackList [] (List.sort packers)
  ++ concatMap (foldl addToPackList []) (map List.sort . groupBy (\x y -> team x == team y) $  breakers)) [1..]
  where
    (breakers, packers) = List.partition (\c -> breakaway c > 0) cyclists
    
    addToPackList :: [Int -> Pack] -> Cyclist -> [Int -> Pack]
    addToPackList [] c =
      [Pack 0 c empty ]
    addToPackList (h:ps) c
      | ((distance c) - (distance leader) < 3) = (Pack tLead c (leader <| cs)):ps
      | otherwise = (Pack 0 c empty):(Pack tLead leader cs):ps
                    where (Pack tLead leader cs _) = h 0
        
-- rotate : TESTED
rotate :: Pack -> Pack
rotate (Pack t lead pack uid) = Pack 0 nLead nPack uid
       where seqDist = fmap distance (pack |> lead)
             (nPack:>nLead) = viewr $ Sequence.zipWith (\c d -> c{distance = d}) (lead <| pack) seqDist
        
packMap :: (Cyclist -> Cyclist) -> Pack -> Pack
packMap f (Pack tLead l p i) = (Pack tLead (f l) (fmap f p) i) 

-- Broken (halfway throught conversion), but probably useless.
-- unpack :: [Pack] -> [Cyclist] 
-- unpack [] = []
-- unpack ((Pack _ leader cs):ps) = (leader:()) ++ (unpack ps)
