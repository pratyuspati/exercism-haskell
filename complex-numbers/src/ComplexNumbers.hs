module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (i, j) = Complex i j

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex i j) = Complex i (-j)

abs :: Floating a => Complex a -> a
abs (Complex i j) = sqrt ((i ** 2) + (j ** 2)) 

real :: Num a => Complex a -> a
real (Complex i _) = i

imaginary :: Num a => Complex a -> a
imaginary (Complex _ j) = j

exp :: Floating a => Complex a -> Complex a
exp (Complex i j) = Complex real comp 
  where 
    real = ((2.718 ** i) * (cos j))
    comp = ((2.718 ** i) * (sin j))

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a1 b1) (Complex a2 b2) = Complex real comp
  where
    real = (a1 * a2) - (b1 * b2)
    comp = (a1 * b2) + (b1 * a2)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a1 b1) (Complex a2 b2) = Complex (a1 + a2) (b1 + b2) 

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a1 b1) (Complex a2 b2) = Complex (a1 - a2) ( b1 - b2) 

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a1 b1) (Complex a2 b2) = Complex real comp
  where
    real = ((a1 * a2) + (b1 * b2)) / (a2^^2 + b2^^2)
    comp = ((b1 * a2) - (a1 * b2)) / (a2^^2 + b2^^2)
