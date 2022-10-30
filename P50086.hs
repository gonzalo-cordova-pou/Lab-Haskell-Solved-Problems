data Queue a = Queue [a] [a]
    deriving (Show)

instance Eq a => Eq (Queue a)
    where
        (Queue l1 l2) == (Queue r1 r2) = if l1++(reverse l2) == r1++(reverse r2) then True else False

instance Functor Queue where
    fmap f (Queue l1 l2) = Queue (fmap f l1) (fmap f l2)

instance Monad Queue where
    return x = Queue [x] []
    x >>= f = fmap f Queue

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter 

create :: Queue a
create = (Queue [] [])

push :: a -> Queue a -> Queue a
push a (Queue l1 l2) = Queue l1 (a:l2)

pop :: Queue a -> Queue a
pop (Queue [] l2) = Queue (reverse $ init l2) []
pop (Queue (x:xs) l2) = Queue xs l2

top :: Queue a -> a
top (Queue [] l2) = last l2
top (Queue (x:xs) _) = x

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

translation :: Num b => b -> Queue b -> Queue b
translation n (Queue l1 l2) = fmap (+ n) (Queue l1 l2)