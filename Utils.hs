module Utils where

import Control.Monad
import Data.Sequence

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)


seqElem :: Eq a => Seq a -> a -> Bool
seqElem seq e = 
  case viewl seq of
    EmptyL -> False
    a :< seq' -> if a == e 
                    then True
                    else seqElem seq' e

partitionM :: (Monad m) => (a -> m Bool) -> Seq a -> m (Seq a, Seq a)
partitionM f empty = return (empty, empty)
partitionM f seq = do
               let (h:<t) = viewl seq
               (true, false) <- partitionM f t
               classification <- f h
               if classification
                  then return (h <| true, false)
                  else return (true, h <| false)
