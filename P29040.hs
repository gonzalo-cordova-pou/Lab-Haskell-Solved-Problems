
-- PROBLEM: https://jutge.org/problems/P25054_en

insert :: [Int] -> Int -> [Int]
-- given a sorted list and an element, correctly inserts the new element in the list
insert [] x = [x]
insert (y:ys) x
    | x <= y = x : y : ys
    | otherwise = y : insert ys x

isort :: [Int] -> [Int]
-- implements insertion sort using the previous function
isort [] = []
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int] 
remove [] _ = []
remove (x:xs) n
    | x == n = xs
    | otherwise = x:(remove xs n)

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs)
    | x > max = x
    | otherwise = max
    where
        max = myMaximum xs

ssort :: [Int] -> [Int]
-- implements selection sort using the previous function
ssort [x] = [x]
ssort l = min:(ssort(remove l min))
    where
        min = minimum l

merge :: [Int] -> [Int] -> [Int]
-- given two sorted lists, merges them to get a list with all the elements in sorted order
merge [] [] = []
merge [] [a] = [a]
merge [a] [] = [a]
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


half :: [Int] -> ([Int], [Int])
half [] = ([], [])
half [x] = ([x], [])
half xs = ((take s xs), (drop s xs))
    where
        s = (length xs ) `div` 2


msort :: [Int] -> [Int]
msort []  = []
msort [x] = [x]
msort l =  merge (msort h1) (msort h2)
    where
        (h1, h2) = half l


