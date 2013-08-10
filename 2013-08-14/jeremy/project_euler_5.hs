
gcd' n 0 = n
gcd' n m = gcd' m (mod n m)

lcm' n m = n * m `div` (gcd' n m)

problem_5 = foldl1 lcm' [1..20]

p5 = foldl1 (\ n m -> div (n * m) (g n m)) [1..20]
  where g n m = if m == 0 then n else g m (mod n m)
