import Control.Monad.Random

import Cyclist
import Pack
import Population
import  Simulation

test :: IO ()
test = do
     c <- genCyclistsIO 2 2 avg
     print c
     p <- evalRandIO . do_breakaway . Pack $ c
     print . unpack $ p
     print (length c)
     print (length . unpack $ p)
     return ()
