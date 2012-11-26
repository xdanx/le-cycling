{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
import Control.Monad.Random
import Control.Monad.Trans.Class
import Data.IORef
import Data.List
import Graphics.Rendering.OpenGL

import Graphics.UI.GLUT
import Graphics.SimplePlot
import System.Environment
import System.Exit
import System.Mem

import Cyclist
import Parser
import Population
import Rendering
import Simulation

time = 100
validOption = ["-X", "-P"]

main :: IO ()
main = do
     progname <- getProgName
     args <- getArgs
     let (opt, rest) = partition ((=='-'). head) args 
         [graphics, plt] = map (flip elem opt) $ validOption
         correct = (length rest == 1) && (and . map (flip elem validOption) $ opt)
     if not correct
       then usage
       else return ()
     if graphics
        then
       do
         initialize progname []
         window <- createWindow progname
         clear [ColorBuffer]
        else return ()
     r <- (genRace (head rest) >>= newIORef)
     if graphics
       then 
         do
           render r
           displayCallback $= (render r)
           actionOnWindowClose $= ContinueExectuion
           addTimerCallback time (loop_wrapper r)
           mainLoop
       else
          do
            g <- getStdGen
            runRandT (looper r) g >>= return . fst
     (Race _ _ _ _ leader_board) <- readIORef r
     print leader_board
     if plt
            then plot X11 $ Data2D [Style Graphics.SimplePlot.Lines, Title "Classment agains team cooperation probability", Graphics.SimplePlot.Color Graphics.SimplePlot.Blue] [] (zip [1..] (map (teamCProb . fst) leader_board))
            else return True
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

looper :: IORef Race -> RandT StdGen IO ()
looper ref = do 
  r <- lift . readIORef $ ref
  n <- loop r
  lift . writeIORef ref $ n
  case n of
    (Race _ _ [] [] win) -> return ()
    otherwise -> looper ref
  
usage :: IO ()
usage = do
  putStrLn "Usage: Cycling [args] [input file]"
  putStrLn "args:"
  putStrLn "\t-P: Plot cooperation against position at end of simulation"
  putStrLn "\t-X: Show real-time graphical representation of race"
  exitFailure
