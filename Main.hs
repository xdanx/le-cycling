{-# LANGUAGE DoAndIfThenElse #-}
import Control.Monad.Random

import Cyclist
import Population
import Simulation

teams = 10 :: Int
team_size = 15 :: Int

main :: IO ()
main = do
     evalRandIO (genCyclists teams team_size avg >>= (mainLoop 0))
     return ()

mainLoop :: Int -> [Cyclist] -> Rand StdGen [Cyclist]
mainLoop n pop = do
                      npop <- turn (n `mod` 5 == 0) pop
                      if(or $ map (\m -> end > distance m) npop)
                      	then mainLoop (n+1) npop
                      	else return npop
