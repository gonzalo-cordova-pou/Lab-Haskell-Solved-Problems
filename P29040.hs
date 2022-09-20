
-- PROBLEM: https://jutge.org/problems/P29040_en

insert :: [Int] -> Int -> [Int]
-- given a sorted list and an element, correctly inserts the new element in the list
insert [] x = [x]
insert (y:ys) x
    | x <= y = x : y : ys
    | otherwise = y : (insert ys x)

isort :: [Int] -> [Int]
-- implements insertion sort using the previous function
isort [] = []
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int] 
remove [] _ = []
remove (x:xs) n
    | x == n = xs
    | otherwise = x:(remove xs n)

ssort :: [Int] -> [Int]
-- implements selection sort using the previous function
ssort [] = []
ssort l = x : (ssort (remove l x)) where x = minimum l

merge :: Ord a => [a] -> [a] -> [a]
-- given two sorted lists, merges them to get a list with all the elements in sorted order
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


half :: [Int] -> ([Int], [Int])
half l = (take n l, drop n l)
    where
        n = (div (length l) 2)


msort :: [Int] -> [Int]
msort []  = []
msort [x] = [x]
msort l =  merge (msort h1) (msort h2)
    where
        (h1, h2) = half l

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = (qsort lower)++[x]++(qsort higher)
    where
        lower = [a | a <- xs, a <= x]
        higher = [a | a <- xs, a > x]

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x:xs) = (genQsort lower)++[x]++(genQsort higher)
    where
        lower = [a | a <- xs, a <= x]
        higher = [a | a <- xs, a > x]