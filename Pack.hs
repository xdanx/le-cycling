module Pack where

import Control.Monad.Random
import Cyclist
import Data.List
import Data.Sequence

data Pack = Pack Int Cyclist (Sequence Cyclist)
            deriving(Show)

{-instance Show Pack where
         show (Pack l) = show $ map (distance) l1-}

getPacks :: [Cyclist] -> [Pack]
getPacks cyclists =
  foldl addToPackList [] (sort packers)
  ++ concatMap (foldl addToPackList []) (map sort . groupBy (\x y -> team x == team y) $  breakers)
  where
    (breakers, packers) = partition (\c -> breakaway c > 0) cyclists
    
    addToPackList :: [Pack] -> Cyclist -> [Pack]
    addToPackList [] c =
      [Pack 0 c empty]
    addToPackList ((Pack tLead leader cs):ps) c = 
      | (distance c) - (distance leader) < 3 = (Pack tLead c (leader <| cs)):ps
      | otherwise = (Pack 0 c empty):(Pack tLead leader cs):ps
        
        
-- Broken (halfway throught conversion), but probably useless.
-- unpack :: [Pack] -> [Cyclist] 
-- unpack [] = []
-- unpack ((Pack _ leader cs):ps) = (leader:()) ++ (unpack ps)
