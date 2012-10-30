module Pack where

import Control.Monad.Random
import Cyclist
import Data.List

import Race

data Pack = Pack [Cyclist]
     deriving(Show)

{-instance Show Pack where
         show (Pack l) = show $ map (distance) l1-}

getPacks :: Race -> [Pack]
getPacks (Race _ len cyclists _) =
  map (\a -> Pack [a]) sprinters
  ++ foldl addToPackList [] (sort packs) 
  ++ concatMap (foldl addToPackList []) (map sort . groupBy (\x y -> team x == team y) $  breakers)
  where
    (sprinters, cyclists') = break (\c -> distance c > (fromIntegral (len - 5))) cyclists
    (breakers, packs) = partition (\c -> breakaway c > 0) cyclists'
    addToPackList :: [Pack] -> Cyclist -> [Pack]
    addToPackList [] c =
      [Pack [c]]
    addToPackList ((Pack (h:cs)):ps) c
      | (distance c) - (distance h) < 3 = ((Pack (c:h:cs)):ps)
      | otherwise = (Pack [c]):(Pack (h:cs)):ps
                    

unpack :: [Pack] -> [Cyclist]
unpack [] = []
unpack ((Pack p):ps) = p ++ (unpack ps)
