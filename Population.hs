{- Population.hs
File containing the possible distributions used for the probabilities
in the default cooperation function.
-}
module Population where

data Population = Population {pmaxs :: (Double, Double), coops :: (Double, Double), energylims :: (Double, Double)}

avg :: Population
avg = Population {pmaxs = (2500, 30), coops = (0.48, 0.21), energylims = (200000, 300)}
