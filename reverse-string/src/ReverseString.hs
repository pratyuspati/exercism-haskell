module ReverseString (reverseString) where

reverseString :: String -> String
reverseString str
  | null str  = ""
  | otherwise = last str : reverseString ( init str )
