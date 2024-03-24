module Pangram (isPangram) where

isPangram :: String -> Bool
isPangram text = and ( zipWith (||) uppers lowers ) 
  where 
    uppers = (`elem` text) <$> ['A' .. 'Z']
    lowers = (`elem` text) <$> ['a' .. 'z']
