module Isogram (isIsogram) where
import Data.Char (isLetter, toLower)
import Data.List (sort, group)

isIsogram :: String -> Bool
isIsogram s = and $ ( (==1).length <$> ( group.sort $ [ toLower x | x <- s, isLetter x ] ) )
