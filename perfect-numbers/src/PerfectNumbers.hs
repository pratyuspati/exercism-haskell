module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

getFactors :: Int -> [Int]
getFactors x = [ a | a <- [1..(x-1)], x `mod` a == 0 ]

calcAliquotSum :: Int -> Int
calcAliquotSum x = sum $ getFactors x

classify :: Int -> Maybe Classification
classify x
  | x <= 0               = Nothing         
  | calcAliquotSum x == x = Just Perfect
  | calcAliquotSum x <  x = Just Deficient
  | calcAliquotSum x >  x = Just Abundant
  | otherwise             = Nothing
