module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | (length xs) /= (length ys) = Nothing
  | otherwise                  = Just $ sum [ 1 | (x,y) <- (zip xs ys), x /= y ]
