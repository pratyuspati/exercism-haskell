module RunLength (decode, encode) where

readInt :: String -> Int
readInt = read

isDigitChar :: Char -> Bool
isDigitChar a = a `elem` ['0' .. '9']

decode :: String -> String
decode encodedText
  | encodedText == ""              = ""
  | isDigitChar (head encodedText) = (replicate l h   ) <> decode t 
  | otherwise                      = (head encodedText) : (decode (tail encodedText))
  where
    l = readInt (takeWhile isDigitChar encodedText)
    h = head $ dropWhile isDigitChar encodedText
    t = tail $ dropWhile isDigitChar encodedText

encode :: String -> String
encode text
  | text == "" = ""
  | l == 1     = (head text)     : (encode $ tail text)
  | otherwise  = (show l) <> [a] <> encode t 
  where
    a = head text
    h = takeWhile (== a) text
    t = dropWhile (== a) text
    l = length h
        
