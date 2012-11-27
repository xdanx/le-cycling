module Utils where

import Control.Monad
import Data.Sequence

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)


seqElem :: Eq a => a -> Seq a -> Bool
seqElem seq e = 
  case viewl seq of
    EmptyL -> False
    a :< seq' -> if a == e then True else seqElem e seq'
