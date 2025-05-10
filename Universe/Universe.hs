{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Universe.Universe where

import qualified Universe.Database.DBMS as DBMS
import Universe.Line
import Universe.Axis
import Universe.Space    (Point)

import GHC.Generics
import Control.DeepSeq      (NFData)

import Data.List    (nub)

data Properties = Properties 
    { energy :: Energy 
    , others :: [Char] } deriving (Show, Generic)
 -- , mass   :: double } deriving Show

instance NFData Properties

instance Eq Properties where 
    (==) a b = (energy a, others a) == (energy b, others b)

data Object = Object 
    { point      :: Point 
    , properties :: Properties } deriving (Show, Generic)

instance NFData Object

instance Eq Object where 
    (==) a b = (point a) == (point b)

instance Ord Object where
    (<)  a b = point a < point b
    (>)  a b = point a > point b
    (<=) a b = a < b || a == b
    (>=) a b = a > b || a == b

data Coord = Coord 
    { number :: Double 
    , plane  :: [Object] } deriving Show

instance Eq Coord where
    (==) a b = (number a == number b)

instance Ord Coord where
    (<)  a b = (number a < number b)
    (>)  a b = (number a > number b)
    (<=) a b = a < b || a == b
    (>=) a b = a > b || a == b

instance Semigroup Coord where
    (<>) (Coord c0 p0) (Coord c1 p1) = Coord c0 (nub $ p0 ++ p1)

type Energy = (Double, Double, Double)

move :: Object -> Object
move obj =
    let (x,y,z) = point $ obj
        (a,b,c) = energy . properties $ obj
     in Object (reposition (x+a,y+b,z+c)) . Properties (redirection (x+a,y+b,z+c) (a,b,c)) . others . properties $ obj

redirection :: Point -> Energy -> Energy
redirection (x,y,z) (kx,ky,kz)
    | x > 0.5 || x < (-0.5) = (-kx,ky,kz)
    | y > 0.5 || y < (-0.5) = (kx,-ky,kz)
    | z > 0.5 || z < (-0.5) = (kx,ky,-kz)
    | otherwise         = (kx,ky,kz)

reposition :: Point -> Point
reposition (x,y,z) = 
    let newX | x > 0.5 = 0.5 | x < (-0.5) = (-0.5) | otherwise = x
        newY | y > 0.5 = 0.5 | y < (-0.5) = (-0.5) | otherwise = y
        newZ | z > 0.5 = 0.5 | z < (-0.5) = (-0.5) | otherwise = z
     in (newX, newY, newZ)

