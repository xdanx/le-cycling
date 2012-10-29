{-# LANGUAGE DoAndIfThenElse #-}
import Control.Monad.Random
import Control.Monad.Trans.Class
import System.Environment
import System.Mem

import Cyclist
import Population
import Rendering
import Simulation

teams = 10 :: Int
team_size = 10 :: Int
race_length = 160000 :: Int

main :: IO ()
main = do
     c <- genCyclistsIO teams team_size avg
     g <- getStdGen
     (Race race_time _ _ leader_board) <- evalRandT (loop $ Race 0 race_length c []) g
     print leader_board
     return ()
     

loop :: Race -> RandT StdGen IO Race
loop r@(Race _ _ [] _) = return r
loop r = turn r >>= (\r -> (lift $ render r) >> lift performGC >> loop r)

{-loop :: Int -> [Cyclist] -> Rand StdGen [Cyclist]
loop n pop = do
                      npop <- turn (n `mod` 5 == 0) pop
                      if(or $ map (\m -> end > distance m) npop)
                      	then mainLoop (n+1) npop
                      	else return npop


      progname <- getProgName
     args <- initialize progname []
     window <- createWindow progname
     s <- get initialWindowSize
     clear [ColorBuffer]
     c <- genCyclistsIO 5 10 avg
     cyclists <- newIORef . flip (Race 500) [] . map (\(x, d) -> x{distance = d}) . zip c $ [1,10..]
     displayCallback $= (get initialWindowSize >>= render cyclists)
     reshapeCallback $= Just (render cyclists)
     sequence . repeat $ (render cyclists s  >> getLine)
     --     evalRandIO (genCyclists teams team_size avg >>= (loop 0))
     destroyWindow window-}
