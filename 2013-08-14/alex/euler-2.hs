#!/usr/bin/env runhaskell

main :: IO ()
main = do
  let limited = takeWhile (<= 4000000) fibs
  let result = sum (filter even limited)
  putStrLn (show result)

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
