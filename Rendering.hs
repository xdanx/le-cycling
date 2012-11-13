{-# LANGUAGE TupleSections #-}
module Rendering where

import Control.Arrow
import Data.IORef
import Data.Tuple
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Cyclist
import Simulation

render :: (IORef Race) -> IO ()
render r = do
       (Race trn len racers sprint finish) <- readIORef r
       clear [ColorBuffer]
       let ys = map distance (racers ++ sprint)
           poss = map ( swap . ((fromRational . toRational) *** (fromRational . toRational)) . (0,) . (subtract 1) . (*2) . (/(fromIntegral len))) ys :: [(GLdouble, GLdouble)]
       renderPrimitive Points $ mapM_ (vertex . uncurry Vertex2) poss
       flush
       return ()
