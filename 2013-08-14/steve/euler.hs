-- problem 2

even_fib_sum :: Int -> Int -> Int -> Int
even_fib_sum previous_value current_value current_sum
  | current_value >= 4000000 = current_sum
  | current_value `mod` 2 == 0 = even_fib_sum current_value next_value (current_sum + current_value)
  | otherwise = even_fib_sum current_value next_value current_sum
  where next_value = previous_value + current_value

-- problem 5

smallest_div_by_20 =
  head [number | number <- [20,40..], all (\divisor -> number `mod` divisor == 0) [1..20]]
