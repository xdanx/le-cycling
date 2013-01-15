{- Stats.hs
Defines probability distributions, to use with the default cooperation function.
-}

module Stats where

import Control.Monad.Random
import Control.Monad.Trans

-- Normal distribution
normal :: (MonadRandom m) => (Double, Double) -> m Double
normal (mean, distr) = do
       x <- getRandom
       y <- getRandom
       return $ mean + (sqrt(-2*log x)*cos(2*pi*y) * distr)
