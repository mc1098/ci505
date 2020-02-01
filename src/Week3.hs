module Week3 where

{-In the lecture on algebraic data types, the type of natural numbers based
  on a successor function was introduced. In this system, every number is either
  zero (Z) or the successor of another number. Thus, the number one, (S Z), is
  the successor of (or is one greater than) zero. This number system is known as
  Peano (pronounced pee-ah-no) numbers.
  Definitions for adding and subtracting Nats were given in the lecture notes.
  If we make Nat an instance of the Num typeclass, we can use Nat values wherever
  a numeric type is expected. In order to do this we have to provide definitions
  for the following functions:
  • Addition and subtraction (given in the lecture notes).
  • Multiplication: we can define this using addition. To multiply two Nats,
  n and m, add m to itself n times.
  • negate, which negates a Nat. The negation of Z is Z. There are no negative
  Peano numbers, so negating one which is not zero should return an error.
  • signum, which returns the sign of a number. For “ordinary” numbers,
  signum returns -1 for negative numbers, 0 for zero and 1 for positive
  numbers. Yours should do the same, except that you only need to consider
  zero and positive numbers.
  • abs, which returns the absolute value of a number. Since there are no
  negative Peano numbers, you should just return the number itself.
  • fromInteger, which converts an Integer to a Nat.-}

data Nat = Z | S Nat deriving (Show)

toInt :: Nat -> Int
toInt = go 0
  where
    go n Z      = n
    go n (S x)  = go (n+1) x



add :: Nat -> Nat -> Nat
add Z x = x
add (S x) y = add x (S y)

sub :: Nat -> Nat -> Nat
sub Z _ = Z
sub x Z = x
sub (S x) (S y) = sub x y

mult :: Nat -> Nat -> Nat
mult Z x = Z
mult (S Z) x = x
mult _ Z = Z
mult x (S Z) = x
mult (S (S x)) y = add (add y y) (mult x y)

{-To make Nat an instance of Num, copy the definition of Nat, add and sub
  into your script-}
instance Num Nat where 
  (+) = add 
  (-) = sub
  (*) = mult
  negate Z = Z 
  negate _ = error "negate called on non-zero Nat"
  signum Z = 0
  signum _ = 1
  abs = id 
  fromInteger = go Z
    where
      go x 0 = x
      go x n = go (S x) (n-1)
