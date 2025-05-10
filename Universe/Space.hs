module Universe.Space where

import qualified Universe.Database.DBMS as DBMS
import Data.Time.Clock
import Universe.Axis

newtype Space a = Space { dimension :: (Axis a, Axis a, Axis a) }

instance (Show a) => Show (Space a) where
    show (Space (x,y,z)) =
        show x ++ "\n" ++
        show y ++ "\n" ++
        show z

type Point = (Double,Double,Double)

getTime :: IO Double
getTime = do
    curretTime <- getCurrentTime
    return
        . (/1e11) . fromIntegral
        . diffTimeToPicoseconds $ utctDayTime curretTime

getModule :: Point -> Double
getModule (x,y,z) = sqrt $ (x^2) + (y^2) + (z^2)

getDotProduct :: Point -> Point -> Double
getDotProduct (x,y,z) (i,j,k) = (x*i) + (y*j) + (z*k)

getAngleBetween :: Point -> Point -> Double
getAngleBetween (x,y,z) (i,j,k) = 
    let mod1 = getModule (x,y,z)
        mod2 = getModule (i,j,k)
        prod = getDotProduct (x,y,z) (i,j,k)
     in acos (prod / (mod1 * mod2))
    
getAngleX :: Point -> Double
getAngleX (x,y,z)
    | x < 0  && y < 0 = (+) (pi) $ getAngleBetween (-1,0,0) (x,y,0)
    | x > 0  && y < 0 = (+) (pi) $ getAngleBetween (-1,0,0) (x,y,0)
    | x == 0 && y < 0 = (+) (pi) $ getAngleBetween (-1,0,0) (x,y,0)
    | otherwise      = getAngleBetween (1,0,0) (x,y,0)

getAngleY :: Point -> Double
getAngleY (x,y,z)
    | y < 0  && z < 0 = (+) (pi) $ getAngleBetween (0,-1,0) (0,y,z)
    | y > 0  && z < 0 = (+) (pi) $ getAngleBetween (0,-1,0) (0,y,z)
    | y == 0 && z < 0 = (+) (pi) $ getAngleBetween (0,-1,0) (0,y,z)
    | otherwise       = getAngleBetween (0,1,0) (0,y,z)

getAngleZ :: Point -> Double
getAngleZ (x,y,z)
    | z < 0  && x < 0 = (+) (pi) $ getAngleBetween (0,0,-1) (x,0,z)
    | z > 0  && x < 0 = (+) (pi) $ getAngleBetween (0,0,-1) (x,0,z)
    | z == 0 && x < 0 = (+) (pi) $ getAngleBetween (0,0,-1) (x,0,z)
    | otherwise       = getAngleBetween (0,0,1) (x,0,z)

type Rad = Double

rotateZ :: Point -> Rad -> Point
rotateZ (x,y,z) n = if x == 0 && y == 0 then (0,0,z) else
    let angle = getAngleX (x,y,z)
        range = getModule (x,y,0)
     in ((*) range $ cos (angle +n), (*) range $ sin (angle +n), z)
    
rotateY :: Point -> Rad -> Point
rotateY (x,y,z) n = if x == 0 && z == 0 then (0,y,0) else
    let angle = getAngleZ (x,y,z)
        range = getModule (x,0,z)
     in ((*) range $ sin (angle +n), y, (*) range $ cos (angle +n))
    
rotateX :: Point -> Rad -> Point
rotateX (x,y,z) n = if y == 0 && z == 0 then (x,0,0) else
    let angle = getAngleY (x,y,z)
        range = getModule (0,y,z)
     in (x, (*) range $ cos (angle +n), (*) range $ sin (angle +n))

