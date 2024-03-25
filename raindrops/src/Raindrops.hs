module Raindrops (convert) where

addToStringIfDiv :: Int -> String -> (Int, String) -> String
addToStringIfDiv n s (d, p) = if n `mod` d == 0 then s <> p else s

convert :: Int -> String
convert n = if null res then show n else res
  where
    res = foldl (addToStringIfDiv n) "" $ zip [3, 5, 7] ["Pling", "Plang", "Plong"]
