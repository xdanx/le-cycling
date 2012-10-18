module Pack where

import Cyclist
import Data.List

data Pack = Pack [Cyclist]

getPacks :: [Cyclist] -> [Pack]
getPacks cyclists =
  foldl addToPackList [] (sort cyclists) 
  where 
    addToPackList :: [Pack] -> Cyclist -> [Pack]
    addToPackList [] c = 
      [Pack [c]]
    addToPackList ((Pack []):ps) c =
      (Pack [c]):ps
    addToPackList ((Pack (h:cs)):ps) c 
      | (distance h) - (distance c) < 3 = ((Pack (c:h:cs)):ps)
      | otherwise = (Pack [c]):(Pack (h:cs)):ps
