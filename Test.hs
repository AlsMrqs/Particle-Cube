module Test where

import qualified Universe.Database.DBMS as DBMS
import Universe.Universe
import Universe.Space
import Universe.Line
import Universe.Axis

import System.Random
import Data.List        (foldl')

genSpace :: Int -> IO (Space (Line Coord), DBMS.Tree Object)
genSpace n = do
    let space = Space (x0, y0, z0)
        x0    = X $ Node (Coord 0 []) Void Void
        y0    = Y $ Node (Coord 0 []) Void Void
        z0    = Z $ Node (Coord 0 []) Void Void
    database <- fmap (snd . genDatabase) $ genObject n
    let newSpace = foldl (\acc x -> insertSpace x acc) space (DBMS.toList database)
    return (newSpace, database)

insertSpace :: Object -> Space (Line Coord) -> Space (Line Coord)
insertSpace obj spc = 
    let getX = (\(x,_,_) -> x) 
        getY = (\(_,y,_) -> y) 
        getZ = (\(_,_,z) -> z) 
        x0   = fmap (insert (Coord (getX $ point obj) [obj])) . getX $ dimension spc
        y0   = fmap (insert (Coord (getY $ point obj) [obj])) . getY $ dimension spc
        z0   = fmap (insert (Coord (getZ $ point obj) [obj])) . getZ $ dimension spc
     in Space (x0,y0,z0)

genDatabase :: [Object] -> ([Object], DBMS.Tree Object)
genDatabase = foldl' (\(lst, db) x -> 
    let (true, dbup) = DBMS.insert x db
     in if true 
        then (lst, dbup) 
        else (x:lst, dbup)) ([], DBMS.Empty)

genObject :: Int -> IO [Object]
genObject n = sequence . take n . cycle $ [randObject] 
    
randDouble :: IO Double
randDouble = rand >>= \n -> (rand >>= return . (/ (100)) . (-) n)
    where
        rand = newStdGen >>= return . fst . random

highDouble :: IO Double
highDouble = rand >>= \n -> (rand >>= return . (/ 1) . (-) n)
    where
        rand = newStdGen >>= return . fst . random

readjust :: Double -> Double
readjust i | i > 0.5 = 0.5 | i < (-0.5) = (-0.5) | otherwise = i

randObject :: IO Object
randObject = do
    x  <- highDouble >>= return . readjust
    y  <- highDouble >>= return . readjust 
    z  <- highDouble >>= return . readjust 
    kx <- randDouble >>= return . readjust 
    ky <- randDouble >>= return . readjust 
    kz <- randDouble >>= return . readjust 
    return . Object (x,y,z) $ Properties (kx,ky,kz) ""

