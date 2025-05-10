module Control where

import Graphics.UI.GLUT
import Control.Concurrent

data Mouse = Mouse
    { click  :: (Double,Double) 
    , posit  :: (Double,Double) 
    , memory :: (Double,Double)
    } deriving Show

getMemory :: Mouse -> (Double,Double)
getMemory (Mouse (a,b) (x,y) (n,m)) = (((-b+y)/100),((-a+x)/100))
    -- todo : memory / (2*pi) -- to strip a large number

keyboardMouse :: KeyboardMouseCallback
keyboardMouse key keyState _ pos = do
    case (key, keyState) of
        (Char 'q', Down) -> leaveMainLoop
        (Char ' ', Down) -> putStrLn "space!"
        _                -> return ()

draggingHandler :: MVar Mouse -> Position -> IO ()
draggingHandler mouseMVar (Position x y) = do
    putStrLn $ "Arrastando mouse em: " ++ show (x, y)

    mouse <- readMVar mouseMVar
    swapMVar mouseMVar $ Mouse (click mouse) (fromIntegral x, fromIntegral y) (getMemory mouse)
    print =<< readMVar mouseMVar -- print --
    postRedisplay Nothing
    return ()

mouseHandler :: MVar Mouse -> MouseButton -> KeyState -> Position -> IO ()
mouseHandler mouseMVar button state pos = do
    putStrLn $ "Mouse Button: " ++ show button
    putStrLn $ "State: "        ++ show state
    putStrLn $ "Position: "     ++ show pos

    --mouse <- takeMVar mouseMVar
    let f = (\(Position a b) -> (fromIntegral a,fromIntegral b))
    mouse <- readMVar mouseMVar
    swapMVar mouseMVar $ Mouse (f pos) (f pos) (getMemory mouse)
    print =<< readMVar mouseMVar

