module Coop where

import Control.Monad.Random

standardCoop :: (MonadRandom m) => Double -> m Bool
standardCoop d = getRandom >>= return . (<d)
