module Darts (score) where

distFromO :: Float -> Float -> Float
distFromO x y = sqrt $ ( x ** 2 ) + ( y ** 2)

score :: Float -> Float -> Int
score x y
  | distFromO x y > 10 =  0 
  | distFromO x y >  5 =  1
  | distFromO x y >  1 =  5
  | otherwise          = 10
