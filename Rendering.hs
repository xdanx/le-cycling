module Rendering where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Race

render :: Race -> IO ()
render = const (return ())
