module Yacht (yacht, Category(..)) where
import Data.List (group, sort)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht Ones           d = sum [ 1 | a <- d, a == 1 ]
yacht Twos           d = sum [ 2 | a <- d, a == 2 ]
yacht Threes         d = sum [ 3 | a <- d, a == 3 ]
yacht Fours          d = sum [ 4 | a <- d, a == 4 ]
yacht Fives          d = sum [ 5 | a <- d, a == 5 ]
yacht Sixes          d = sum [ 6 | a <- d, a == 6 ]
yacht FullHouse      d = if all (`elem` (length <$> group (sort d))) [2, 3] then sum d else 0
yacht FourOfAKind    d = if any (>= 4) (length <$> group (sort d)) then sum (tail (sort d)) else 0
yacht LittleStraight d = if all (`elem` d) [1 .. 5] then 30 else 0
yacht BigStraight    d = if all (`elem` d) [2 .. 6] then 30 else 0
yacht Choice         d = sum d
yacht Yacht          d = if all (== (head d)) d then 50 else 0    
