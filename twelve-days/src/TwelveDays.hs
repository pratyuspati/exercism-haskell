module TwelveDays (recite) where

events :: [String]
events = ["a Partridge in a Pear Tree",
          "two Turtle Doves"         ,
          "three French Hens"        ,
          "four Calling Birds"       ,
          "five Gold Rings"          ,
          "six Geese-a-Laying"       ,
          "seven Swans-a-Swimming"   ,
          "eight Maids-a-Milking"    ,
          "nine Ladies Dancing"      ,
          "ten Lords-a-Leaping"      ,
          "eleven Pipers Piping"     ,
          "twelve Drummers Drumming" ]

days :: [String]
days = ["first"   ,
        "second"  ,
        "third"   ,
        "fourth"  ,
        "fifth"   ,
        "sixth"   ,
        "seventh" ,
        "eighth"  ,
        "ninth"   ,
        "tenth"   ,
        "eleventh",
        "twelfth" ]


joinGrammatically :: [String] -> String
joinGrammatically e
  | l == 1    = head e <> "."
  | l == 2    = (head e) <> ", and " <> (last e) <> "."
  | otherwise = (head e) <> ", "     <> (joinGrammatically $ tail e)
  where l = length e

genLineDayI :: Int -> String
genLineDayI i = "On the " <> (days !! (i-1)) <> " day of Christmas my true love gave to me: " <> gifts
  where gifts = joinGrammatically $ reverse (take i events)

recite :: Int -> Int -> [String]
recite start stop = genLineDayI <$> [start .. stop]
