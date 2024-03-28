module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, vectorOf, chooseInt)
import Data.List       (sort)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier x = (x-10) `div` 2

ability :: Gen Int
ability = sum.tail.sort <$> ( vectorOf 4 (chooseInt (1, 6)))

charFromList :: [Int] -> Character
charFromList [st, de, co, int, wi, ch] = Character st de co int wi ch (10 + modifier co)
charFromList _                         = Character 0  0  0  0   0  0  0

character :: Gen Character
character = charFromList <$> (vectorOf 6 ability)	

