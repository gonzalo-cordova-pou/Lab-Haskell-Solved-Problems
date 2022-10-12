
sumMultiples35 :: Integer -> Integer
sumMultiples35 n = (sum_mul 3 n) + (sum_mul 5 n) - (sum_mul 15 n)
    where 
        sum_mul a n = a * n' * (n'+1) `div` 2
            where n' = (n-1) `div` a

next_tripl :: (Int, Integer, Integer) -> Int -> (Int, Integer, Integer)
next_tripl (a, b, c) n = if a <= n then next_tripl (a+1, b+c, b) n else (a, b, c)

third_el :: (a, b, c) -> c
third_el (a, b, c) = c

fourth_el :: (a, b, c, d) -> d
fourth_el (a, b, c, d) = d

fibonacci :: Int -> Integer
fibonacci n = third_el (next_tripl (1, 1, 0) n)

new_next_tripl :: (Integer, Integer, Integer, Integer) -> Integer -> (Integer, Integer, Integer, Integer)
new_next_tripl (a, b, c, d) n = if b < n then new_next_tripl (a+1, b+c, b, (suma b d)) n else (a, b, c, d)
    where
        suma b d = if even b then d+b else d

sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis n = fourth_el (new_next_tripl (1, 1, 0, 0) n)

isqrt n = (floor . sqrt . fromIntegral) n

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = True
isPrime n
    | (length [x | x <- [2 .. (isqrt n)], mod n x == 0]) > 0 = False
    | otherwise = True

isFactor :: Int -> Int -> Bool
isFactor x y = x `mod` y == 0 

largestPrimeFactor :: Int -> Int
largestPrimeFactor n = head $ filter (isPrime) (filter (isFactor n) (fromNto0 n))
    where
        fromNto0 n = takeWhile (>0) (iterate (\x -> x - 1) n)

isPalindromic :: Integer -> Bool
isPalindromic n = all (==True) (zipWith (==) (show n) (reverse (show n)))