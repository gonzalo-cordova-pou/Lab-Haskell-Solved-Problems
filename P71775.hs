
countIf :: (Int -> Bool) -> [Int] -> Int
countIf f l = length (filter f l)

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam l [] = []
pam l (x:xs) = [(map x l)] ++ (pam l xs)

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 [] fs = []
pam2 (x:xs) fs = [f x | f <- fs]: (pam2 xs fs)

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl f f2 n l = foldl f2 n (filter f l)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f [] n = [n]
insert f (x:xs) n
    | f x n = x:(insert f xs n)
    | otherwise = n:(x:xs)

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f [] = []
insertionSort f [x] = [x]
insertionSort f (x:xs) = insert f (insertionSort f xs) x