module IsbnVerifier (isbn) where
import Data.Maybe (fromJust, isNothing)

readInt :: String -> Int
readInt = read

toNum :: (Char, Int) -> Maybe Int
toNum (a, i)
  | a == 'X' && i == 10   = Just $ 10
  | a `elem` ['0' .. '9'] = Just $ readInt [a] 
  | otherwise             = Nothing

isbn :: String -> Bool
isbn s 
  | length filt /= 10 = False
  | any isNothing num = False
  | otherwise         = (==0).((flip mod) 11) $ sum $ (uncurry (*)) <$> (zip (reverse [1 .. 10]) (fromJust <$> num))
  where
    filt = [ i | i <- s, i /= '-' ]
    l    = length filt
    num  = toNum <$> (zip filt [1 .. l])

