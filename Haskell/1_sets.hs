{- PURELY FUNCTIONAL INT SETS! -}

type Set = Int -> Bool

--Indicates whether a set contains a given element.
contains :: (Int -> Bool) -> Int -> Bool
contains set x = set x

--Returns the set of the one given element.
singletonSet :: Int -> Set
singletonSet x = (\y -> y == x)

--Returns the union of the two given sets
union :: Set -> Set -> Set
union set1 set2 x = set1 x || set2 x

--Returns the intersection of the two given sets
intersect :: Set -> Set -> Set
intersect set1 set2 x = set1 x && set2 x

--Returns the difference of the two given sets,
diff :: Set -> Set -> Set
diff set1 set2 x = set1 x && not (set2 x)

--Given a set s and a property p Int->Bool
--returns the subset of `s` for which `p` holds.
inRange :: Int -> Set -> [Int]
inRange b s = [x | x <- [-b..b], s x]

setfilter :: Set -> (Int -> Bool) -> Set
setfilter s f = (\x -> s x && f x)


--Given a bound b a set s and a property 'p', 
--returns whether all integers 
--in the range [-b,b[ within `s` satisfy `p`.
forall :: Int -> Set -> (Int -> Bool) -> Bool
forall b set p = aux $ -b
 where aux y | y == b = True
             | (intersect set p) y = aux $ y+1 
             | (not (set y) )= aux $ y+1
             | otherwise = False 


--Given a bound b a set s and a property 'p', 
--returns whether there exists a bounded integer within `s`
--that satisfies `p`.
exists :: Int -> Set -> (Int -> Bool) -> Bool
exists b set p = not ( forall b set (\x -> not (p x) ) ) 
--exists b s p= not $ forall b s $ not.p


