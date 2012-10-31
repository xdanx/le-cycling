{-# LANGUAGE DoAndIfThenElse #-}
import Control.Monad.Random
import Control.Monad.Trans.Class
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Environment
import System.Exit
import System.Mem

import Cyclist
import Population
import Rendering
import Simulation

teams = 10 :: Int
team_size = 10 :: Int
race_length = 160000 :: Int
time = 1000

main :: IO ()
main = do
     progname <- getProgName
     args <- initialize progname []
     window <- createWindow progname
     clear [ColorBuffer]
     c <- genCyclistsIO teams team_size avg
     r <- newIORef (Race 0 race_length c [] [])
     render r
     displayCallback $= (render r)
     addTimerCallback time (loop_wrapper r)
     mainLoop

loop_wrapper :: IORef Race -> IO ()
loop_wrapper ref = do
             r <- readIORef ref
             g <- getStdGen
             nr <- evalRandT (loop r) g 
             case nr of
                  (Race _ _ [] [] win) -> print win >> exitSuccess
                  otherwise -> do
                                                writeIORef ref nr
                                                render ref
                                                addTimerCallback time (loop_wrapper ref)
             

loop :: Race -> RandT StdGen IO Race
loop r = do
     n@(Race trn _ _ _ _) <- turn r
     lift performGC
     lift $ putStrLn ("turn: " ++ show trn)
     return n

--     turn r >>= (\n -> (lift $ render n) >> lift performGC >> (lift $ putStrLn ("turn: " ++ show (trn + 1))) >> loop n)

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
