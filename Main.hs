{-# LANGUAGE DoAndIfThenElse, TupleSections #-}

import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.Class
import Data.IORef
import Data.List
import Graphics.Rendering.OpenGL
import Graphics.SimplePlot
import Graphics.UI.GLUT
import System.Environment
import System.Exit
import System.Mem

import Cyclist
import Parser
import Population
import Rendering
import Simulation

time = 100 :: Int
validOption = ["-X", "-P"] :: [String]

main :: IO ()
main = do
     progname <- getProgName
     args <- getArgs
     let (opt, rest) = partition ((=='-'). head) args 
         [graphics, plt] = map (flip elem opt) $ validOption
         correct = (length rest == 1) && (and . map (flip elem validOption) $ opt)
     unless correct usage
     when graphics $ do
                        initialize progname []
                        createWindow progname
                        clear [ColorBuffer]
     ref <- (genRace (head rest) >>= newIORef)
     if graphics
       then 
         do
           render ref
           displayCallback $= render ref
           actionOnWindowClose $= ContinueExectuion
           addTimerCallback time $ loop graphics ref
           mainLoop
       else loop graphics ref
     (Race _ _ _ _ leader_board) <- readIORef ref
     print leader_board
     when plt . void . plot X11 . Data2D [Style Graphics.SimplePlot.Lines, Title "Classment agains cooperation probability", Graphics.SimplePlot.Color Graphics.SimplePlot.Blue] [] . zip [1..] . map (genCProb . fst) $ leader_board
     exit

loop :: Bool -> IORef Race -> IO ()
loop rend ref = do
  r <- readIORef ref
  n <- getStdGen >>= evalRandT (turn r)
  writeIORef ref n
  when rend $ render ref
  case n of
    (Race _ _ [] [] _) -> when rend leaveMainLoop
    (Race _ _ _ _ _) -> (if rend then addTimerCallback time else id) $ loop rend ref
  
usage :: IO ()
usage = do
  putStrLn "Usage: Cycling [args] [input file]"
  putStrLn "args:"
  putStrLn "\t-P: Plot cooperation against position at end of simulation"
  putStrLn "\t-X: Show real-time graphical representation of race"
  exitFailure
