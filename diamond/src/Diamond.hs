module Diamond (diamond) where

mirrorAndConcat :: [a] -> [a]
mirrorAndConcat x = init x ++ reverse x

applyMask :: Char -> Bool -> Char
applyMask c True  = c
applyMask _ False = ' '

genLine :: Int -> Char -> Int -> String
genLine len c i = mirrorAndConcat $ zipWith applyMask word [ p == i | p <- [1 .. len]]
  where
    word = replicate len c

genDiamond :: Char -> [String]
genDiamond c = mirrorAndConcat $ zipWith ( genLine len ) word ( reverse [1 .. len] ) 
  where
    word = ['A' .. c]
    len  = length word

diamond :: Char -> Maybe [String]
diamond c
  | c `elem` ['A' .. 'Z'] = Just $ genDiamond c
  | otherwise             = Nothing
