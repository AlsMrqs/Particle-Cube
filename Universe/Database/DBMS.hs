module Universe.Database.DBMS where 

-- { Data.Map [+] Data.Set } --

data Tree a = Node a (Tree a) (Tree a)
            | Empty
    deriving Show

toList :: (Ord a) => Tree a -> [a]
toList Empty = []
toList (Node x l r) = (toList l) ++ [x] ++ (toList r)

find :: (Eq a, Ord a) => a -> Tree a -> Maybe a
find _ (Empty)      = Nothing
find k (Node x l r)  
    | x == k = Just x 
    | x <  k = find k r
    | k <  x = find k l

insert :: (Eq a, Ord a) => a -> Tree a -> (Bool, Tree a)
insert k (Empty)      = (True, Node k Empty Empty)
insert k (Node x l r) 
    | x == k = (False, Node x l r)
    | x <  k = (\(a,b) -> (a, Node x l b)) (insert k r)
    | k <  x = (\(a,b) -> (a, Node x b r)) (insert k l)

