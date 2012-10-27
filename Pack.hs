module Pack where

import Control.Monad.Random
import Cyclist
import Data.List

data Pack = Pack [Cyclist]

instance Show Pack where
         show (Pack l) = show $ map (distance) l

getPacks :: [Cyclist] -> [Pack]
getPacks cyclists =
  foldl addToPackList [] (sort (filter (\c -> breakaway c == 0) cyclists)) 
  ++ concatMap (\t -> foldl addToPackList [] (sort (filter (\c -> breakaway c > 0 && team c == t) cyclists))) [1..10]
  where
    addToPackList :: [Pack] -> Cyclist -> [Pack]
    addToPackList [] c = 
      [Pack [c]]
    addToPackList ((Pack (h:cs)):ps) c 
      | (distance c) - (distance h) < 3 = ((Pack (c:h:cs)):ps)
      | otherwise = (Pack [c]):(Pack (h:cs)):ps
                    

unpack :: [Pack] -> [Cyclist]
unpack [] = []
unpack ((Pack p):ps) = p ++ (unpack ps) 
