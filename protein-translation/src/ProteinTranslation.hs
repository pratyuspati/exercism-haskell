module ProteinTranslation(proteins) where
import Data.Maybe (fromJust)

getProtein :: String -> Maybe String
getProtein x
  | x `elem` ["AUG"]                      = Just "Methionine"
  | x `elem` ["UUU", "UUC"]               = Just "Phenylalanine"
  | x `elem` ["UUA", "UUG"]               = Just "Leucine"
  | x `elem` ["UCU", "UCC", "UCA", "UCG"] = Just "Serine"
  | x `elem` ["UAU", "UAC"]               = Just "Tyrosine"
  | x `elem` ["UGU", "UGC"]               = Just "Cysteine"
  | x `elem` ["UGG"]                      = Just "Tryptophan"
  | x `elem` ["UAA", "UAG", "UGA"]        = Just "STOP"
  | otherwise                             = Nothing

proteins :: String -> Maybe [String]
proteins p
  | p == ""                = Just []
  | protein == Nothing     = Nothing
  | protein == Just "STOP" = Just []
  | otherwise              = Just [fromJust protein]  <> (proteins $ drop 3 p)
  where protein = getProtein $ take 3 p
