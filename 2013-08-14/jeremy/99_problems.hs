
-- For #24:
import System.Random

-- Problem 8

compress :: (Eq a) => [a] -> [a]
compress (a:b:rest)
  | a == b = compress (b:rest)
  | otherwise = a : (compress (b:rest))
compress lst = lst

-- Problem 19

rotate :: [a] -> Int -> [a]
rotate lst n = (drop distance lst) ++ (take distance lst)
  where distance = mod n (length lst)

-- Problem 24

selectNWithRandom :: [a] -> Int -> StdGen -> [a]
selectNWithRandom _   0 _    = []
selectNWithRandom lst n rand = picked : (selectNWithRandom rest (n - 1) nextRand)
  where (index, nextRand) = randomR (0, (length lst) - 1) rand
        rest              = (take index lst) ++ (drop (index + 1) lst)
        picked            = lst !! index

rndSelect :: [a] -> Int -> [a]
rndSelect items count = selectNWithRandom items count (mkStdGen 42)

-- Problem 31

isPrime :: (Integral a) => a -> Bool
isPrime n = n > 1 && (null $ filter (divides n) [2..n-1])
  where divides n k = (mod n k) == 0

-- Problem 32

gcd' :: (Integral a) => a -> a -> a
gcd' n 0 = n
gcd' n m = gcd' m (mod n m)