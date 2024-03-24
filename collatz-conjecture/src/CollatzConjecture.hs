module CollatzConjecture (collatz) where

calcCollatz :: Integer -> Integer
calcCollatz a
  | even a = div a 2
  | odd  a = 3 * a + 1
  
collatzIterVal :: Integer -> Integer -> Integer
collatzIterVal iter 1 = iter
collatzIterVal iter n = collatzIterVal (iter + 1) (calcCollatz n)

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (collatzIterVal 0 n)
