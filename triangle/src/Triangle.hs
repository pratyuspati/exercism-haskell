module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | a + b <= c                 = Illegal
  | b + c <= a                 = Illegal
  | a + c <= b                 = Illegal
  | a == b && b == c           = Equilateral
  | a == b || b == c || a == c = Isosceles
  | otherwise                  = Scalene
