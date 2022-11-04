import Data.List

degree :: Eq a => [(a, a)] -> a -> Int
degree [] _ = 0
degree ((v1,v2):xs) v
    | (v1 == v) || (v2 == v) = 1 + (degree xs v)
    | otherwise = (degree xs v)

degree' :: Eq a => [(a, a)] -> a -> Int 
degree' l c = sum [1 | (v1, v2) <- l, (v1 == c) || (v2 == c)]

neighbors :: Ord a => [(a, a)] -> a -> [a]
neighbors l c = sort [if v1 == c then v2 else v1 | (v1, v2) <- l, (v1 == c) || (v2 == c)]