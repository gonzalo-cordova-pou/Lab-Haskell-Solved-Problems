
data Tree a = Empty | Node a (Tree a) (Tree a)
         deriving (Show)

instance Foldable Tree where
    foldr _ b Empty = b
    foldr f b (Node x l r) = foldr f (foldr f (f x b) l) r

avg :: Tree Int -> Double
avg t = (fromIntegral (foldr (+) 0 t)) / size
    where
        size = (fromIntegral (foldr (\x y -> y + 1) 0 t))

cat :: Tree String -> String
cat Empty = ""
cat t = tail (foldr (\x y -> y ++ " " ++ x) "" t)
