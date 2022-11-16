
-- 'Rate': the sampling rate (as an int), in units of samples per second
-- 'Samples': a list containing samples, where each sample is a double
type Rate = Int
type Samples = [Double]
type Sound = (Rate, Samples)

empty :: Sound
empty = (0, [])

backwards :: Sound -> Sound
backwards (r,s) = (r, reverse s)

mix :: Sound -> Sound -> Double -> Sound
mix (r1,s1) (r2,s2) p 
 | p < 0 || p > 1 || r1 /= r2 = empty
 | otherwise = (r1, s)
  where s = zipWith (\ x y -> p * x + (1 - p) * y) s1 s2

