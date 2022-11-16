
type Set = Int -> Bool

contains :: Set -> Int -> Bool
contains set x = set x

emptySet :: Set
emptySet = \x -> False

singletonSet :: Int -> Set
singletonSet x = (\y -> y == x)

union :: Set -> Set -> Set
union set1 set2 = (\x -> set1 x || set2 x)

intersect :: Set -> Set -> Set
intersect set1 set2 = (\x -> set1 x && set2 x)

complement :: Set -> Set
complement set = (\x -> not (set x))

diff :: Set -> Set -> Set
diff set1 set2 = (\x -> set1 x && not (set2 x))
-- diff set1 set2 = intersect set1 (complement set2)

-- U = {0, 1, ..., n}
n = 100 

size :: Set -> Int
size set = aux 0
    where aux i | i == n+1 = 0
                | set i = 1 + aux (i+1)
                | otherwise = aux (i+1)


issubset :: Set -> Set -> Bool
issubset s1 s2 = aux 0
    where aux i | i == n+1 = True
                | not (s1 i) = aux (i+1)
                | s2 i = aux (i+1)
                | otherwise = False 


forall :: Set -> (Int -> Bool) -> Bool
forall set p = aux 0
 where aux i | i == n+1 = True
             | intersect set p i = aux (i+1) 
             | not (set i) = aux (i+1)
             | otherwise = False 

exists :: Set -> (Int -> Bool) -> Bool
exists set p = not (forall set (not.p))


