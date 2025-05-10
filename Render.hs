module Render where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Control.Concurrent.MVar
import Control.Concurrent
import Control.DeepSeq
import qualified Data.Vector as Vector

import qualified Universe.Universe as Universe
import Universe.Space
import Control

render :: MVar (Vector.Vector Universe.Object) -> MVar Mouse -> Double -> DisplayCallback
render mvarObj mvarMouse t = do
    diff  <- getTime >>= return . (\x -> (x-t)/10)
    obj   <- readMVar mvarObj
    mouse <- readMVar mvarMouse
    let (x,y) = getMemory mouse
        obj00 = Universe.Object (0,0,0) $ Universe.Properties (0.01,0,0) ""
    clear [ColorBuffer]

    let vectorOfPoints = Vector.map Universe.point obj

    color $ Color3 1.0 1.0 (1.0 :: GLfloat)
    renderPrimitive Points $!
        Vector.mapM_ ( draw . pointToGLPoint . redim_ . updateView (x,y). (flip rotateX) (diff/8) . (flip rotateY) (diff/9) . (flip rotateZ) (diff/(-9))) 
            $! deepseq vectorOfPoints vectorOfPoints 

    swapBuffers >> postRedisplay Nothing

redim_ :: Point -> Point
redim_ (x,y,z) =
    let f = 5.5
        factor  = 1 + (z/f)
        newX = x * factor
        newY = y * factor
     in (newX, newY, z)

updateView :: (Double,Double) -> Point -> Point
updateView (x,y) = flip rotateY y . flip rotateX x

draw :: (GLfloat,GLfloat,GLfloat) -> DisplayCallback
draw = \(x,y,z) -> vertex $ Vertex3 x y z

pointToGLPoint :: Point -> (GLfloat,GLfloat,GLfloat)
pointToGLPoint (x,y,z) = (realToFrac(x), realToFrac(y), realToFrac(z)) :: (GLfloat,GLfloat,GLfloat)


