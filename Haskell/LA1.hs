
type Vector = [Int]
type Matrix = [Vector]

identity :: Int -> Matrix
identity 1 = [[1]]
identity n = [ x ++ [0] | x <- identity (n-1)] ++ [replicate (n-1) 0 ++ [1]]

isSquare :: Matrix -> Bool
isSquare m = length m == length (head m)

transpose :: Matrix -> Matrix
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

dot :: Vector -> Vector -> Int
dot [] [] = 0
dot (x:xs) (y:ys) = x*y + dot xs ys 
dot _ _ = error "not the same length"

addVec :: Vector -> Vector -> Vector
addVec [] [] = []
addVec (x:xs) (y:ys) = (x+y: addVec xs ys)
addVec _ _ = error "not the same length"

addMat :: Matrix -> Matrix -> Matrix
addMat [] [] = []
addMat (x:xs) (y:ys) = (addVec x y: addMat xs ys)
addMat _ _ = error "not the same length"

mulVecConst :: Int -> Vector -> Vector
mulVecConst i v = map (*i) v

mulMatConst :: Int -> Matrix -> Matrix
mulMatConst i m = map (mulVecConst i) m

mulMatVec :: Matrix -> Vector -> Vector
mulMatVec m v =  [dot m1 v| m1 <- m] 

mulMatMat :: Matrix -> Matrix -> Matrix 
mulMatMat m1 m2 = transpose [mulMatVec m1 m| m <- m2]

diagonal :: Vector -> Matrix
diagonal xs = [ (replicate i 0) ++ [x] ++ (replicate (n - 1 - i) 0) | (i,x) <- zip [0..(n-1)] xs]
 where n = length xs

determinant :: Matrix -> Int
determinant [[x]] = x
determinant mat = sum [ (sign i) * a * determinant (getRest i mat)| (i,a) <- zip [1..(length mat)] (head mat)]
 where getRest i mat = transpose [ tail x | (j,x) <- zip [1..(length mat)] (transpose mat), i /= j]
       sign i = if mod i 2 == 1 then 1 else -1 

kronecker :: Matrix -> Matrix -> Matrix
kronecker m1 m2 = [ concat [ mulVecConst x b | x <- a] | a <- m1, b <- m2]




