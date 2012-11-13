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
import Parser
import Population
import Rendering
import Simulation

teams = 1 :: Int
team_size = 2 :: Int
race_length = 10000 :: Int
time = 100

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
     actionOnWindowClose $= ContinueExectuion
     addTimerCallback time (loop_wrapper r)
     mainLoop
     (Race _ _ _ _ leader_board) <- readIORef r
     print leader_board
     exit

loop_wrapper :: IORef Race -> IO ()
loop_wrapper ref = do
             r <- readIORef ref
             g <- getStdGen
             nr <- evalRandT (loop r) g 
             writeIORef ref nr  
             render ref
             case nr of
                  (Race _ _ [] [] win) -> leaveMainLoop
                  otherwise -> addTimerCallback time (loop_wrapper ref)
             

loop :: Race -> RandT StdGen IO Race
loop r = do
     n@(Race trn _ _ _ _) <- turn r
     lift performGC
     return n
