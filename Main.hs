import Control.Monad.Random
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Environment

import Cyclist
import Population
import Rendering
import Simulation

teams = 10 :: Int
team_size = 15 :: Int

main :: IO ()
main = do
     {-     progname <- getProgName
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
     return ()

{-loop :: Int -> [Cyclist] -> Rand StdGen [Cyclist]
loop n pop = do
                      npop <- turn (n `mod` 5 == 0) pop
                      if(or $ map (\m -> end > distance m) npop)
                      then loop (n+1) npop
                      else return npop-}
