
-- PROBLEM: https://jutge.org/problems/P77907_en


absValue :: Int -> Int
    -- retorna el valor absolut d'un enter

absValue n
    | n >= 0    = n
    | otherwise = -n


power :: Int -> Int -> Int
    -- retorna la pot de x^p

power _ 0 = 1
power x p = x * power x (p - 1)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n
    | (length [x | x <- [2 .. (isqrt n)], mod n x == 0]) > 0 = False
    | otherwise = True


slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib(n-1) + slowFib(n-2)

third_el :: (Int, Int, Int) -> Int
third_el (x,y,z) = z

next_triplet :: (Int, Int, Int) -> Int -> (Int, Int, Int)
next_triplet (x,y,z) n = if x < n  then next_triplet(x+1, z, y + z) n else (x, y, z)

quickFib :: Int -> Int
quickFib 0 = 0
quickFib n = third_el (next_triplet (1,0,1) n)