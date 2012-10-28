module Rendering where

import Control.Arrow
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Cyclist
import Simulation

render :: (IORef Race) -> Size -> IO ()
render r (Size x y) = do
       print "Call back motherfuckers...."
       (Race trn len racers finish) <- readIORef r
       clear [ColorBuffer]
       let ys = map distance racers
           poss = map ( ((fromRational . toRational) *** (fromRational . toRational)) . (0.5,) . (subtract 1) . (*2) . (/(fromIntegral len))) ys :: [(GLdouble, GLdouble)]
       renderPrimitive Points $ mapM_ (vertex . uncurry Vertex2) poss
       flush
       return ()
       

