module Population where

data Population = Population {max10s :: (Double, Double), coops :: (Double, Double)}

avg = Population {max10s = (7.1, 0.4), coops = (0.48, 0.21)}
