-- PROBLEM: https://jutge.org/problems/P93588_en

myMap :: (a -> b) -> [a] -> [b]
-- emulates map using comprehension lists
myMap f a = [f(x) | x <- a]

myFilter :: (a -> Bool) -> [a] -> [a]
-- emulates filter using comprehension lists
myFilter f a = [x | x <- a,  f(x)]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- emulates zipWith using comprehension lists and zip
myZipWith f a b = [f x y | (x, y) <- zip a b]

thingify :: [Int] -> [Int] -> [(Int, Int)]
-- returns the list that pairs the elements if the element of the second list divides the one in the first list
thingify a b = [(x, y) |  x <- a, y <- b,  x `mod` y == 0]

factors :: Int -> [Int]
-- generates the ordered list with all its factors (non necessaryly primes)
factors n = [x | x <- [1..n], n `mod` x == 0]
