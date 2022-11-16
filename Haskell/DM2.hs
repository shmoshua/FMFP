
type Set a = [a] 

contains :: Eq a => Set a -> a -> Bool
contains set x = x `elem` set

removeDup :: Eq a => Set a -> Set a
removeDup [] = []
removeDup (x:xs) | x `elem` xs = removeDup xs
                 | otherwise = x:removeDup xs

emptySet :: Set a
emptySet = []

equals :: Eq a => Set a -> Set a -> Bool
equals s1 s2 = and [x `elem` s2 | x <- s1]

intersect :: Eq a => Set a -> Set a -> Set a
intersect s1 s2 = [x | x <- s1, x `elem` s2]

diff :: Eq a => Set a -> Set a -> Set a
diff s1 s2 = [x | x <- s1, not (x `elem` s2)]

union :: Eq a => Set a -> Set a -> Set a
union s1 s2 = removeDup (s1++s2)

cartesianProduct :: Eq a => Set a -> Set b -> Set (a,b)
cartesianProduct s1 s2 = [(x,y)| x <- s1, y <- s2]

issubset :: Eq a => Set a -> Set a -> Bool
issubset s1 s2 = and [x `elem` s2| x <- s1]

isRelation :: (Eq a, Eq b) => Set a -> Set b -> Set (a,b) -> Bool
isRelation s1 s2 p = issubset p $ cartesianProduct s1 s2

identity :: Set a -> Set (a,a)
identity u = [(x,x) | x <- u]

isReflexive :: Eq a => Set a -> Set (a,a) -> Bool
isReflexive u p = issubset (identity u) p

isSymmetric :: Eq a => Set (a,a) -> Bool
isSymmetric p = and [(y,x) `elem` p|(x,y) <- p]

isAntisymmetric :: Eq a => Set a -> Set (a,a) -> Bool
isAntisymmetric u p = issubset (intersect p p') (identity u)
    where p' = [(b,a)|(a,b) <- p]

power :: Eq a => Set (a,a) -> Int -> Set (a,a)
power p 1 = p
power p i = aux (power p (i-1)) p
    where aux p1 p2 = removeDup [(a,d)| (a,b) <- p1, (c,d) <- p2, b == c]

isTransitive :: Eq a => Set (a,a) -> Bool
isTransitive p = issubset (power p 2) p

fixedPoint :: Eq a => (Set a -> Set a) -> Set a -> Set a
fixedPoint f p
  | p' `equals` p = p'
  | otherwise = fixedPoint f p'
  where p' = f p

transitiveClosure :: Eq a => Set (a,a) -> Set (a,a)
transitiveClosure p = fixedPoint combine p
 where combine x = union p [(a,d)| (a,b) <- p, (c,d) <- x, b == c]
       
