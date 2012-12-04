module Population where

data Population = Population {pmaxs :: (Double, Double), coops :: (Double, Double), energylims :: (Double, Double)}

avg :: Population
avg = Population {pmaxs = (1.6, 1), coops = (0.48, 0.21), energylims = (200, 30)}
