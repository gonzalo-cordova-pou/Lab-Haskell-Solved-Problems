
-- PROBLEM: https://jutge.org/problems/P37072_en


data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ b c) = 1 + size b + size c


height :: Tree a -> Int
height Empty = 0
height (Node _ b c) = 1 + max (height b) (height c)


equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty (Node _ _ _) = False
equal (Node _ _ _ ) Empty = False
equal (Node a1 b1 c1) (Node a2 b2 c2) = a1 == a2 && 
                                        equal b1 b2 && 
                                        equal c1 c2


isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty (Node _ _ _) = False
isomorphic (Node _ _ _ ) Empty = False
isomorphic (Node a1 b1 c1) (Node a2 b2 c2) = a1 == a2 && 
                                        ((equal b1 b2 && 
                                        equal c1 c2) ||
                                        (equal b1 c2 && 
                                        equal c1 c1))

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a b c) = [a] ++ preOrder b ++ preOrder c

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a b c) = postOrder b ++ postOrder c ++ [a]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a b c) = inOrder b ++ [a]  ++ inOrder c

breadthFirst' :: [Tree a] -> [a]
breadthFirst' [] = []
breadthFirst' (Empty:xs) = breadthFirst' xs
breadthFirst' ((Node a b c):xs) = a : breadthFirst' (xs ++ [b,c])

breadthFirst :: Tree a -> [a] 
breadthFirst Empty = []
breadthFirst t = breadthFirst' [t]

build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build (x:xs) l = Node x (build left_pre left_in) (build right_pre right_in)
    where 
        left_in = takeWhile (/=x) l
        right_in = tail (dropWhile (/=x) l)
        left_pre = take (length left_in) xs
        right_pre = drop (length left_in) xs

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ a Empty = a
overlap _ Empty b = b
overlap f (Node a1 b1 c1) (Node a2 b2 c2) = (Node (f a1 a2) (overlap f b1 b2) (overlap  f c1 c2))