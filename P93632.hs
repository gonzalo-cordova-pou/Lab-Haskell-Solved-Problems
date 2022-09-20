eql :: [Int] -> [Int] -> Bool
-- Bool that tells wether two lists of integers are equal.
eql a b = (length a == length b) && all (==True) (zipWith (==) a b)

prod :: [Int] -> Int
-- returns the product of a list of integers
prod l = foldl (*) 1 l

prodOfEvens :: [Int] -> Int
-- returns the product of all even numbers of a list of integers
prodOfEvens l = prod (filter even l)

powersOf2 :: [Int]
-- generates the list of all the powers of 2
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
-- returns the dot product of two lists of float numbers with the same size.
scalarProduct a b = foldl (+) 0 (zipWith (*) a b)