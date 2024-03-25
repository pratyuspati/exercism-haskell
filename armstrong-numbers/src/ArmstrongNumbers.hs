module ArmstrongNumbers (armstrong) where

toDigits :: Integral a => a -> [a]
toDigits x
  | x >= 10   = toDigits (x `div` 10) ++ [(x `mod` 10)]
  | otherwise = [x]

calcArmstrong :: Integral a => a -> a
calcArmstrong x = sum $ ( ^ len ) <$> xd
  where
    xd  = toDigits x
    len = length xd

armstrong :: Integral a => a -> Bool
armstrong a = a == calcArmstrong a
