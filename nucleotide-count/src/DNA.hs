module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, empty, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

updateMap :: Either String (Map Nucleotide Int) -> Maybe Nucleotide -> Either String (Map Nucleotide Int)
updateMap (Right m) (Just a) = Right $ insertWith (+) a 1 m 
updateMap (Right m) Nothing  = Left "error"
updateMap (Left  e) _        = Left e

getNucleotideFromChar :: Char -> Maybe Nucleotide
getNucleotideFromChar 'A' = Just A
getNucleotideFromChar 'C' = Just C
getNucleotideFromChar 'G' = Just G
getNucleotideFromChar 'T' = Just T
getNucleotideFromChar  _  = Nothing

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldl updateMap initMap $ map getNucleotideFromChar xs
  where initMap = Right empty
