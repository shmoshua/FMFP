
type Matrix = [[Int]]

transpose :: Matrix -> Matrix
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- addMat :: Matrix -> Matrix -> Matrix

main = putStrLn "Hello, World!"