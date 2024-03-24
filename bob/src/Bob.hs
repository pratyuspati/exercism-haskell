module Bob (responseFor) where
import Data.Char (isLetter, isUpper, isSpace)

stripWhiteSpace :: String -> String
stripWhiteSpace xs = [ x | x <- xs, not $ isSpace x ]

isQuestion :: String -> Bool
isQuestion xs = last xt == '?' where xt = stripWhiteSpace xs

getLetters :: String -> String
getLetters xs = [ x | x <- xs, isLetter x ]

isYelling  :: String -> Bool
isYelling  xs = 
  not (null xl) && all isUpper xl
  where xl = getLetters xs

isSilence :: String -> Bool
isSilence xs = null xt where xt = stripWhiteSpace xs

responseFor :: String -> String
responseFor xs
  | isSilence xs                  = "Fine. Be that way!"                 -- Silence
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"  -- YELL a question
  | isQuestion xs                 = "Sure."                              -- A question
  | isYelling xs                  = "Whoa, chill out!"                   -- YELLING
  | otherwise                     = "Whatever."                          -- \_O_/
