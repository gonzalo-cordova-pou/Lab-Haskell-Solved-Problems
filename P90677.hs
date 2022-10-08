
-- PROBLEM: https://jutge.org/problems/P90677_en


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a [] = a
myFoldl f a (x:xs) =  myFoldl f (f a x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f a [] = a
myFoldr f a (x:xs) =  f x (foldr f a xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : (myIterate f (f x))

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil b f x= if b x == False then  myUntil b f (f x) else x

myMap :: (a -> b) -> [a] -> [b]
myMap f l = foldr (\x y -> f x : y) [] l

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter b l = foldr aux [] l
    where aux x y = if b x then x:y else y

myAll :: (a -> Bool) -> [a] -> Bool
myAll b l = and (map b l)

myAny :: (a -> Bool) -> [a] -> Bool
myAny b l = or (map b l)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : (myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f x y = map (aux f) (myZip x y)
    where aux f (a, b) = f a b