{-# LANGUAGE DoAndIfThenElse, TupleSections #-}

import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.Class
import Data.IORef
import Data.List
import Data.Word
import Graphics.Rendering.OpenGL
import Graphics.SimplePlot
import Graphics.UI.SDL hiding (flip)
import System.Environment
import System.Exit
import System.Mem

import Cyclist
import Parser
import Population
import Rendering
import Simulation

time = 100 :: Word32
validOption = ["-X", "-P"] :: [String]

main :: IO ()
main = withInit [InitEverything] $ do
     args <- getArgs   
     let (opt, rest) = partition ((=='-'). head) args 
         [graphics, plt] = map (flip elem opt) $ validOption
         correct = (length rest == 1) && (and . map (flip elem validOption) $ opt)
     unless correct usage
     g <- getStdGen
--     let g = read "710075201 1" :: StdGen
     print g
     ref <- (genRace (head rest) >>= newIORef . (,g))
     rend <- if(graphics)
             then do
              pics@(background,_,_,_) <- loadPics
              screen <- setVideoMode (surfaceGetWidth background) (surfaceGetHeight background) 32 [SWSurface]
              return $ render screen pics (surfaceGetWidth background) ref
              else return $ return ()
     loop rend ref
     (Race _ _ _ _ leader_board, _) <- readIORef ref
     print leader_board
     when plt . void . plot X11 . Data2D [Style Graphics.SimplePlot.Lines, Title "Classment agains cooperation probability", Graphics.SimplePlot.Color Graphics.SimplePlot.Blue] [] . zip [1..] . map (pmax . fst) $ leader_board

loop :: IO () -> IORef (Race, StdGen) -> IO ()
loop rend ref = do
  (r,g) <- readIORef ref
  n@(Race turn len run sprint win, g')  <- runRandT (turn r) g
  writeIORef ref n
  rend
  print r
  case n of
    (Race _ _ [] [] _, _) -> exitSuccess
    (Race _ _ _ _ _, _) -> performGC >> delay 10 >> loop rend ref
  
usage :: IO ()
usage = do
  putStrLn "Usage: Cycling [args] [input file]"
  putStrLn "args:"
  putStrLn "\t-P: Plot cooperation against position at end of simulation"
  putStrLn "\t-X: Show real-time graphical representation of race"
  exitFailure
