module Pack where

import Control.Monad.Random
import Cyclist
import Data.List

data Pack = Pack [Cyclist]

instance Show Pack where
         show (Pack l) = show $ map (distance) l

getPacks :: [Cyclist] -> [Pack]
getPacks cyclists =
  foldl addToPackList [] (sort cyclists) 
  where 
    addToPackList :: [Pack] -> Cyclist -> [Pack]
    addToPackList [] c = 
      [Pack [c]]
    {-addToPackList ((Pack []):ps) c =
      (Pack [c]):ps-}
    addToPackList ((Pack (h:cs)):ps) c 
      | abs((distance h) - (distance c)) < 3 = ((Pack (c:h:cs)):ps)
      | otherwise = (Pack [c]):(Pack (h:cs)):ps
