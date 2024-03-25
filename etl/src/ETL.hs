module ETL (transform) where

import Data.Map (Map, toList, fromList)
import Data.Char (toLower)
shatter :: (a, String) -> [(Char, a)]
shatter (score, letters) = [ (toLower l, score) | l <- letters ]

transform :: Map a String -> Map Char a
transform legacyData = fromList ( concat $ shatter <$> toList legacyData ) 
