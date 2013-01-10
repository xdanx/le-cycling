{- Utils.hs
Utility functions used in various parts of the code.
-}

module Utils where

import Control.Monad
import Control.Monad.Trans
import Data.Sequence
import Data.Unique

-- same as concatMap but for monadic functions.
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)


-- Finds if element e in the sequence seq.
seqElem :: Eq a => Seq a -> a -> Bool
seqElem seq e = 
  case viewl seq of
    EmptyL -> False
    a :< seq' -> if a == e 
                    then True
                    else seqElem seq' e

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

-- Return a new unused ID
newID ::(MonadIO m) => m Int
newID = liftIO (newUnique >>= return . hashUnique)
