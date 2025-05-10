module Universe.Axis where

data Axis a = X a | Y a | Z a
    deriving Show

instance Functor Axis where
    fmap f (X a) = X (f a)
    fmap f (Y a) = Y (f a)
    fmap f (Z a) = Z (f a)

