{- Rendering.hs
Creates the visualisation.
-}
{-# LANGUAGE TupleSections #-}
module Rendering where

import Control.Arrow
import Data.Foldable hiding (mapM_)
import Data.IORef
import Data.Tuple
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Cyclist
import Pack
import Simulation

render :: IORef Race -> IO ()
render r = do
       (Race trn len racers sprint finish) <- readIORef r
       clear [ColorBuffer]
       let ys = map distance ((Prelude.concatMap (toList.getPack) racers) ++ sprint)
           poss = map ( swap . ((fromRational . toRational) *** (fromRational . toRational)) . (0,) . (subtract 1) . (*2) . (/len)) ys :: [(GLdouble, GLdouble)]
       renderPrimitive Points $ mapM_ (vertex . uncurry Vertex2) poss
       flush
       return ()
