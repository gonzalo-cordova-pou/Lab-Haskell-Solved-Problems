data Tree a = Empty | Node a (Tree a) (Tree a)

data Forest a = Forest [Tree a] deriving (Show)

instance Show a => Show (Tree a) where
    show Empty = "()"
    show (Node a l r) = "(" ++ (show l) ++ "," ++ show a ++ "," ++ show r ++ ")"

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Functor Forest where
    fmap f (Forest l) = Forest (map (fmap f) l)

doubleT :: Num a => Tree a -> Tree a
doubleT t = fmap (*2) t

doubleF :: Num a => Forest a -> Forest a
doubleF f = fmap (*2) f
