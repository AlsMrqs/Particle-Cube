module Evolution where

import qualified Universe.Universe as Universe
import qualified Data.Vector as Vector
import Control.Concurrent.MVar
import Control.Concurrent
import Control.DeepSeq
import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Render

updatePosition :: MVar (Vector.Vector Universe.Object) -> IO ()
updatePosition mvarObj = do
    threadDelay 50000 -- Pequeno delay para evitar consumo excessivo
    modifyMVar_ mvarObj $ \obj -> do
        let newUniverse = Vector.map Universe.move obj `using` parTraversable rseq
        newUniverse `deepseq` return newUniverse  -- Força a avaliação antes de substituir
    updatePosition mvarObj

