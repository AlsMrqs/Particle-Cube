module Universe.Line where

import Control.Parallel.Strategies

data Line a = Void | Node a (Line a) (Line a)

instance (Show a) => Show (Line a) where
    show Void = "Void"
    show (Node a l r) = showL l ++ " (...) " ++ show a ++ " (...) " ++ showR r
        where
            showL Void         = "[" 
            showL (Node x l _) = showL l ++ show x 
            showR Void         = "]"
            showR (Node x _ r) = show x ++ showR r

    -- double ptr insert --

--insert :: (Semigroup a, Eq a, Ord a) => a -> Line a -> Line a
--insert _ Void = Void
--insert k (Node x l r)
--    | k == x = let link = Node ((<>) k x) (updateL link l) (updateR link r) in link
--    | k <  x = let link = Node x (insertL (k, link) l) (updateR link r) in link   
--    | k >  x = let link = Node x (updateL link l) (insertR (k, link) r) in link

insert :: (Semigroup a, Eq a, Ord a) => a -> Line a -> Line a
insert _ Void = Void
insert k (Node x l r)
    | k == x = let link = Node ((<>) k x) (updateL link l) (updateR link r) in link
    | k <  x = let link = Node x (insertL (k, link) l) (updateR link r) in link   
    | k >  x = let link = Node x (updateL link l) (insertR (k, link) r) in link

type Link a = Line a

updateR :: Link a -> Line a -> Line a
updateR _    Void         = Void
updateR link (Node x _ r) = newLink
    where
        newLink = Node x link (updateR newLink r)
        
updateL :: Link a -> Line a -> Line a
updateL _    Void         = Void
updateL link (Node x l _) = newLink
    where
        newLink = Node x (updateL newLink l) link

insertR :: (Semigroup a, Eq a, Ord a) => (a, Link a) -> Line a -> Line a
insertR (k, link) Void         = Node k link Void
insertR (k, link) (Node x _ r)
    | k == x = let newLink = Node ((<>) k x) link (updateR newLink r) in newLink
    | k <  x = let newLink = Node k link (insertR (x, newLink) r) in newLink
    | k >  x = let newLink = Node x link (insertR (k, newLink) r) in newLink

insertL :: (Semigroup a, Eq a, Ord a) => (a, Link a) -> Line a -> Line a
insertL (k, link) Void         = Node k Void link
insertL (k, link) (Node x l _)
    | k == x = let newLink = Node ((<>) k x) (updateL newLink l) link in newLink
    | k <  x = let newLink = Node x (insertL (k, newLink) l) link in newLink
    | k >  x = let newLink = Node k (insertL (x, newLink) l) link in newLink

