module Stats where

import Control.Monad.Random

normal :: (Double, Double) -> Rand StdGen Double
normal (mean, distr) = do
       x <- getRandom 
       y <- getRandom
       return $ (mean + sqrt(-2*log x)*cos(2*pi*y)) * distr
