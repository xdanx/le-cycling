{-# LANGUAGE TupleSections #-}
module Rendering where

import Control.Arrow
import Data.Foldable hiding (mapM_, concatMap)
import Data.IORef
import Data.Tuple.Utils
import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Cyclist
import Pack
import Simulation

data CyclistT = Group | Break | Sprinter

render :: Surface -> (Surface, Surface, Surface, Surface) -> Int -> IORef Race -> IO ()
render screen (background, pack, breakaway, sprinter) width r = do
       (Race _ len racers sprint _) <- readIORef r
       let cyclists = (concatMap (\p -> map (Prelude.flip (,) $ case p of
                           Pack {} -> fst3
                           Breakaway {} -> snd3) (toList . getPack $ p)) racers) ++ (map (,thd3) sprint)
       blitSurface background Nothing screen Nothing
       mapM_ (\(c, tp) -> blitCyclist screen (pack, breakaway, sprinter) tp len (distance c) width) cyclists
       Graphics.UI.SDL.flip screen
       return ()
       

blitCyclist :: Surface -> (Surface, Surface, Surface) -> ((Surface,Surface,Surface) -> Surface) -> Double -> Double -> Int -> IO ()
blitCyclist screen pics tp len pos width = do
            let pic = tp pics
            blitSurface pic Nothing screen (Just (Rect {rectX = (floor $ (pos / len) * (fromIntegral width)), rectY = 310, rectW = 0, rectH = 0}))
            return ()

loadPics :: IO (Surface, Surface, Surface, Surface)
loadPics = do
         background <- loadTyped "img/background.png" PNG
         pack <- loadTyped "img/cyclist_black.png" PNG
         breakaway <- loadTyped "img/cyclist_blue.png" PNG
         sprinter <- loadTyped "img/cyclist_green.png" PNG
         return (background, pack, breakaway, sprinter)

{-render :: IORef Race -> IO ()
render r = do
       (Race trn len racers sprint finish) <- readIORef r
       clear [ColorBuffer]
       let ys = map distance ((Prelude.concatMap (toList.getPack) racers) ++ sprint)
           poss = map ( swap . ((fromRational . toRational) *** (fromRational . toRational)) . (0,) . (subtract 1) . (*2) . (/(fromIntegral len))) ys :: [(GLdouble, GLdouble)]
       renderPrimitive Points $ mapM_ (vertex . uncurry Vertex2) poss
       flush
       return ()-}
