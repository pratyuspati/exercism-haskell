module DNA (toRNA) where

toRNASingle :: Char -> Either Char Char
toRNASingle 'C' = Right 'G'
toRNASingle 'G' = Right 'C'
toRNASingle 'T' = Right 'A'
toRNASingle 'A' = Right 'U'
toRNASingle  a  = Left   a

appendToSequence :: Either Char String -> Either Char Char -> Either Char String
appendToSequence (Left  a)      _      = Left  a
appendToSequence (Right _)  (Left  b)  = Left  b
appendToSequence (Right a)  (Right b)  = Right (a ++ [b])

toRNA :: String -> Either Char String
toRNA xs = foldl appendToSequence (Right "") $ map toRNASingle xs 
