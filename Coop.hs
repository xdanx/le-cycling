{- Coop.hs
Contains functions that are used to determine if a Cyclist
is going to act as a cooperator or as a defector.
-}

module Coop where

import Control.Monad.Random

-- Default cooperation strategy. 
-- The cyclist has a probability of cooperation,
-- we draw a random number, if it's smaller than the 
-- probability the cyclist acts as a cooperator.
standardCoop :: (MonadRandom m) => Double -> m Bool
standardCoop d = getRandom >>= return . (<d)
