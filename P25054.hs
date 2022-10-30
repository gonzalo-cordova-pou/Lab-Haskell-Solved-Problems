
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
mySum [] = 0
mySum (x:xs) = x + mySum xs

average :: [Int] -> Float
average l = fromIntegral(mySum l) / fromIntegral(myLength l)

buildPalindrome :: [Int] -> [Int]
buildPalindrome l = reverse l ++ l

remove :: [Int] -> [Int] -> [Int]
remove l [] = l
remove l (x:xs) = remove [i | i <- l, x /= i] xs

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

isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = True
isPrime n = length l == 0
    where
        l = [x | x <- [2..isqrt(n)], mod n x == 0]

primeDivisors :: Int -> [Int]
primeDivisors n = [x | x <- [2..n], isPrime(x) && (mod n x == 0)]