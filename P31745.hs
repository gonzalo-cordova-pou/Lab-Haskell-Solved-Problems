
-- PROBLEM: https://jutge.org/problems/P31745_en

flatten :: [[Int]] -> [Int]
-- flattens a list of lists of integers in a list of integers
flatten []     = []
flatten (x:xs) = x ++ flatten xs

myLength :: String -> Int
-- returns the length of a string
myLength s = foldl (+) 0 (map (const 1) s)

myReverse :: [Int] -> [Int] 
--reverses a list of integers
myReverse l = foldl (flip (:)) [] l

countIn :: [[Int]] -> Int -> [Int]
-- returns the list that tells how many times x appears in each sublist of l
countIn l x =   map (aux x) l
    where aux x = length . filter (==x)

firstWord :: String -> String
firstWord l = takeWhile (/=' ') (dropWhile (==' ') l)