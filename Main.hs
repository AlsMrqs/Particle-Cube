module Main where

import qualified Data.Vector as Vector
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Time.Clock
import Data.Bool            (bool)

import qualified Universe.Universe as Universe
import Universe.Space
import Universe.Axis
import Evolution
import Render
import Test
import Control

main :: IO ()
main = do
    (progName,_) <- getArgsAndInitialize
    _window      <- createWindow "GLUT - learning" 
    currentTime  <- getTime

    objects      <- genObject 800
    mvarObjects  <- deepseq objects $! newMVar $! Vector.fromList objects 

    tid          <- forkIO $ updatePosition mvarObjects 

    rotation     <- newMVar $ Mouse (0,0) (0,0) (0,0)

    --color $ Color3 1.0 1.0 (1.0 :: GLfloat)
    --renderPrimitive Points $! mapM_ (draw . pointToGLPoint) [(0.0,0.0,0.0)]

    windowSize            $= Size 600 500
    displayCallback       $= addTimerCallback 60 (render mvarObjects rotation currentTime)
    keyboardMouseCallback $= Just (keyboardMouse)
    motionCallback        $= Just (draggingHandler rotation)
    mouseCallback         $= Just (mouseHandler rotation)
    mainLoop
    killThread tid

