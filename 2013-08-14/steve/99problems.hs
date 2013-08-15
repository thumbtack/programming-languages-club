import System.Random

-- problem 8
compress lst = (head lst) : [next | (this, next) <- zip lst (tail lst), this /= next]

-- problem 19

rotate lst n
  | n == 0 = lst
  | n > 0 = rotate (tail lst ++ [head lst]) (n - 1)
  | n < 0 = rotate (last lst : init lst) (n + 1)

-- problem 24

rnd_select count maximum = take count (randomRs (1, maximum) (mkStdGen 10))

-- problem 31

is_prime value = all (\x -> value `mod` x /= 0) [2..value - 1]

-- problem 32

my_gcd a b
  | a == b = a
  | a < b = my_gcd a (b - a)
  | otherwise = my_gcd b (a - b)

