module Acronym (abbreviate) where
import Data.Char (toUpper, isLetter, isSpace, isUpper)

keepRelevant :: String -> String
keepRelevant xs = [x | x <- xs, isLetter x || isSpace x || x == '-']

transformToSpace :: Char -> Char
transformToSpace x
  | x == '-'  = ' '
  | otherwise =  x

extractAcronymFromWord :: String -> String
extractAcronymFromWord ""     = ""
extractAcronymFromWord (x:xs) = toUpper x : [a | a <- xs, isUpper a, not (all isUpper xs)]

abbreviate :: String -> String
abbreviate xs = concatMap extractAcronymFromWord (words $ transformToSpace <$> keepRelevant xs)
