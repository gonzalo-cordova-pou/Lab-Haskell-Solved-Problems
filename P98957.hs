-- PROBLEM: https://jutge.org/problems/P98957_en

ones :: [Integer]
ones = 1:ones

nats :: [Integer]
nats = scanl (+) 0 ones

ints :: [Integer]
ints = 0 : merge l [-x| x <- l]
    where
        l = tail nats
        merge (x:xs) ys = x:merge ys xs

triangulars :: [Integer]
triangulars = scanl (+) 0 (tail nats)

factorials :: [Integer]
factorials = scanl (*) 1 (tail nats)

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = garbell (tail (tail nats))
    where
        garbell (p : xs) = p : garbell [x | x <- xs, x `mod` p /= 0]

hammings :: [Integer]
hammings = 1: (map (*2) hammings) `merge` (map (*3) hammings) `merge` (map (*5) hammings)
    where merge (x:xs) (y:ys)
            | x < y = x : merge xs (y:ys)
            | x > y = y : merge (x:xs) ys
            | otherwise = x : merge xs ys


unwrap :: [Char] -> [Char]
unwrap [] = []
unwrap (x:xs) =  show (length (takeWhile (==x) (x:xs))) ++ [x] ++ unwrap (dropWhile (==x) (x:xs))


lookNsay :: [Integer]
lookNsay = iterate count 1
    where count n =  read (unwrap (show n))

pyramid2 :: [Integer] -> [Integer]
pyramid2 l
        | length l == 1 = []
        | otherwise = (sum2 (take 2 l)):(pyramid2 (tail l))
            where sum2 [a,b] = a+b

pyramid :: [Integer] -> [Integer]
pyramid [] = [1]
pyramid l = [1] ++ pyramid2 l ++ [1]

tartaglia :: [[Integer]]
tartaglia = iterate pyramid [1]