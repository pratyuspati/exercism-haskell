module SumOfMultiples (sumOfMultiples) where
import Data.List (union)

getMultiplesLt :: Integer -> Integer -> [Integer]
getMultiplesLt max n = if n == 0 then [] else [n, (2*n) .. (max-1)]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ foldl union [] (map (getMultiplesLt limit) factors)
