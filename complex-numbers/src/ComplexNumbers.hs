module ComplexNumbers
  ( Complex,
    conjugate,
    abs,
    exp,
    real,
    imaginary,
    mul,
    add,
    sub,
    div,
    complex,
  )
where

import Prelude hiding (abs, div, exp)

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving (Eq, Show)

complex :: (a, a) -> Complex a
complex (a, b) = Complex a b

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex a b) = Complex a (-1 * b)

abs :: (Ord a, Floating a) => Complex a -> a
abs (Complex a b) = sqrt (a * a + b * b)

real :: Num a => Complex a -> a
real (Complex a _) = a

imaginary :: Num a => Complex a -> a
imaginary (Complex _ b) = b

exp :: (Ord a, Floating a) => Complex a -> Complex a
exp (Complex a b) = Complex ((2.71828 ** a) * ((cos b) + (sin b))) 0.0

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) = Complex (a * c - b * d) (b * c + a * d)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex (a + c) (b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = Complex (a - c) (b - d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) = Complex ((a * c + b * d) / (c ^ 2 + d ^ 2)) ((b * c - a * d) / (c ^ 2 + d ^ 2))
