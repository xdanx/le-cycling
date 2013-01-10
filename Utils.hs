{- Utils.hs
Utility functions used in various parts of the code.
-}

module Utils where

import Control.Monad
import Data.Sequence

-- same as concatMap but for monadic functions.
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)


-- Finds if element e in the sequence seq.
seqElemWith :: Eq a => Seq a -> a -> (a -> a -> Bool) -> Bool
seqElemWith seq e comp = 
  case viewl seq of
    EmptyL -> False
    a :< seq' -> if comp a e 
                    then True
                    else seqElemWith seq' e comp
                         
seqElem :: Eq a => Seq a -> a -> Bool 
seqElem seq e = seqElemWith seq e (==)

-- same as partition, but with monads.
partitionM :: (Monad m) => (a -> m Bool) -> Seq a -> m (Seq a, Seq a)
partitionM f seq = case (viewl seq) of
               EmptyL -> return (empty, empty)
               (h:<t) -> do
                          (true, false) <- partitionM f t
                          classification <- f h
                          if classification
                             then return (h <| true, false)
                             else return (true, h <| false)
