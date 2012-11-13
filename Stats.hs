module Stats where

import Control.Monad.Random

normal :: (Double, Double) -> Rand StdGen Double
normal (mean, distr) = do
       x <- getRandom
       y <- getRandom
       return $ (mean + sqrt(-2*log x)*cos(2*pi*y)) * distr
       

normalT :: (Monad m) => (Double, Double) -> RandT StdGen m Double
normalT (mean, distr) = do
       x <- getRandom 
       y <- getRandom
       return $ (mean + sqrt(-2*log x)*cos(2*pi*y)) * distr
