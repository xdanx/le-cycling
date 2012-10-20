import Control.Monad.Random

import Cyclist
import Population
import Simulation

size = 150 :: Int

main :: IO ()
main = do
     evalRandIO (genCyclists size avg >>= mainLoop)
     return ()

mainLoop :: [Cyclist] -> Rand StdGen [Cyclist]
mainLoop pop = do
                      npop <- turn pop
                      if(or $ map (\m -> end > distance m) npop)
                      then mainLoop npop
                      else return npop
