{- Rendering.hs
Creates the visualisation.
-}
{-# LANGUAGE TupleSections #-}
module Rendering where

import Control.Arrow
import Data.Foldable hiding (mapM_, concatMap)
import Data.IORef
import Data.Sequence
import Data.Tuple.Utils
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image
import System.IO.Unsafe

import Cyclist
import Pack
import Simulation

data CyclistT = Group | Break | Sprinter

-- import Race

-- background = unsafePerformIO $ loadTyped "background.png" PNG

{-test :: (Surface, Surface, Surface, Surface) -> IO ()
test (background, pack, breakaway, sprinter)
     = withInit [InitEverything] $ do
       screen <- setVideoMode (surfaceGetWidth background) (surfaceGetHeight background) 32 [SWSurface]
       blitSurface background Nothing screen Nothing
       blitSurface pack Nothing screen (Just (Rect {rectX = 0, rectY = 310, rectW = 0, rectH = 0}))
       SDL.flip screen
       delay 2000-}

render :: Surface -> (Surface, Surface, Surface, Surface) -> Int -> IORef Race -> IO ()
render screen (background, pack, breakaway, sprinter) width r = do
       (Race _ len racers sprint _) <- readIORef r
       let cyclists = (concatMap (\p -> map (case p of
                           Pack {} -> (,Group)
                           Breakaway {} -> (,Break)) (toList . getPack $ p)) racers) ++ (map (,Sprinter) sprint)
       mapM_ (\(c, tp) -> blitCyclist screen (pack, breakaway, sprinter) tp len (distance c) width) cyclists
       

blitCyclist :: Surface -> (Surface, Surface, Surface) -> CyclistT -> Double -> Double -> Int -> IO ()
blitCyclist screen pics tp len pos width = do
            let pic = (case tp of 
                            Group -> fst3
                            Break -> snd3
                            Sprinter -> thd3) pics
            blitSurface pic Nothing screen (Just (Rect {rectX = (floor $ (pos / len) * (fromIntegral width)), rectY = 310, rectW = 0, rectH = 0}))
            return ()

loadPics :: IO (Surface, Surface, Surface, Surface)
loadPics = do
         background <- loadTyped "background.png" PNG
         pack <- loadTyped "cyclist_black.png" PNG
         breakaway <- loadTyped "cyclist_blue.png" PNG
         sprinter <- loadTyped "cyclist_green.png" PNG
         return (background, pack, breakaway, sprinter)

{-import Cyclist
import Pack
import Simulation-}

-- render :: IO ()



{-render :: IORef Race -> IO ()
render r = do
       (Race trn len racers sprint finish) <- readIORef r
       clear [ColorBuffer]
       let ys = map distance ((Prelude.concatMap (toList.getPack) racers) ++ sprint)
           poss = map ( swap . ((fromRational . toRational) *** (fromRational . toRational)) . (0,) . (subtract 1) . (*2) . (/len)) ys :: [(GLdouble, GLdouble)]
       renderPrimitive Points $ mapM_ (vertex . uncurry Vertex2) poss
       flush
       return ()
-}
