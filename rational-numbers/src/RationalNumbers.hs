module RationalNumbers
(Rational,
 abs,
 numerator,
 denominator,
 add,
 sub,
 mul,
 div,
 pow,
 expRational,
 expReal,
 rational) where

import Prelude hiding (div, abs, Rational)

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving(Eq, Show)

--  x  |  y  |  l  |  m  --
--  -------------------  --
--  +  |  +  |  0  |  0  --
--  +  |  -  |  1  |  1  --
--  -  |  +  |  0  |  0  --
--  -  |  -  |  1  |  1  --
--  -------------------  --
--  => Toggle if denominator is negative
rational :: Integral a => (a, a) -> Rational a
rational (x, y) = (Rational l m)
  where
    d  = gcd x y
    t  = (y < 0)
    a  = x `quot` d
    l  = if t then (-1) * a else a
    b  = y `quot` d
    m  = if t then (-1) * b else b

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational a b) = Rational c d
  where
    c = if a < 0 then (-a) else a
    d = if b < 0 then (-b) else b

numerator :: Integral a => Rational a -> a
numerator (Rational a _) = a

denominator :: Integral a => Rational a -> a
denominator (Rational _ b) = b

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational a b) (Rational c d) = rational (x, y)
  where
    x = (a * d) + (b * c)
    y = (b * d)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational a b) (Rational c d) = rational (x, y)
  where
    x = (a * d) - (b * c)
    y = (b * d)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational a b) (Rational c d) = rational ((a * c), (b * d))

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational a b) (Rational c d) = rational ((a * d), (b * c))

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational a b) x = if x < 0 then (rational (den, num)) else (rational (num, den))
  where 
    xabs = if x < 0 then (-x) else x
    num  = (a ^ xabs)
    den  = (b ^ xabs)

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational (Rational c d) x = ((fromIntegral c) ** x) / ((fromIntegral d) ** x) 

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal x (Rational a b) = (x ** (fromIntegral a)) ** (1 / fromIntegral b)
