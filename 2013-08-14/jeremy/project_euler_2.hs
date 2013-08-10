
fib = fib' 0 1
  where fib' a b = a : (fib' (a + b) a)

problem2 = sum $ filter even $ takeWhile (<4000000) fib