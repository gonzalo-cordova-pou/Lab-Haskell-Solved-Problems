
-- PROBLEM: https://jutge.org/problems/P25054_en

myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs)
    | x > max = x
    | otherwise = max
    where
        max = myMaximum xs

mySum :: [Int] -> Int
mySum [] = 0s
mySum (x:xs) = x + mySum xs

average :: [Int] -> Float
average l = fromIntegral(mySum l) / fromIntegral(myLength l)

buildPalindrome :: [Int] -> [Int]
buildPalindrome l = reverse l ++ l

removeItem :: [Int] -> Int -> [Int]
removeItem [] _                 = []
removeItem (x:xs) el
    | x == el   = removeItem xs el
    | otherwise = [x] ++ removeItem xs el

remove :: [Int] -> [Int] -> [Int]
remove l [] = l
remove l (x:xs) = remove (removeItem l x) xs

flatten :: [[Int]] -> [Int]
flatten []     = []
flatten (x:xs) = x ++ flatten xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs)
    | mod x 2 /= 0 = (x:ys, zs)
    | otherwise    = (ys, x:zs)
    where
        (ys, zs) = oddsNevens xs

isPrime :: Int -> Bool
isPrime 1 = True
isPrime n = aux(n-1)
    where
        aux d
            | d == 1 = True
            | mod n d == 0 = False
            | otherwise = aux (d-1)

factors :: Integral a => a -> [a]
factors n = [x | x <- [2..n], mod n x == 0]

primeDivisors :: Int -> [Int]
primeDivisors n = filter isPrime (factors n)