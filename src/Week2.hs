module Week2 where

{-This means that all standard functions will be imported from the Prelude
  except ||, && and min, allowing you to write your own versions.)-}
import Prelude hiding ((||),(&&),min)

{-1. Create a function exOr :: Bool -> Bool -> Bool, which acts as an
  “exclusive or” from logic. That is, exOr x y returns true if one of x and
  y is true, but not both.-}
exOr :: Bool -> Bool -> Bool
exOr x y = not x == y

{-2. Create your own version of the logical “or” (||) function. Your function
  should be defined as an infix function which is applied between its two
  arguments, e.g. x || y.-}
(||) :: Bool -> Bool -> Bool
True  || _    = True
_     || True = True
_     || _    = False

{-3. Create your own (infix) version of the logical “and”:-}
infixl 2 &&
(&&) :: Bool -> Bool -> Bool
True  && True  = True
_     && _     = False

{-4. Create a function which takes three Ints and returns true if none of them
  are equal to each other:-}
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z = x /= y && x /= z && y /= z

{-5. Create your own version of the standard function min, which returns the
  smallest of its two arguments. Use this to create a function minThree,
  which returns the smallest of its three arguments.-}
min :: Ord a => a -> a -> a
min x y
  | x < y     = x
  | otherwise = y

minThree :: Ord a => a -> a -> a -> a
minThree x y z = x `min` y `min` z

{-6. Write the function avgThree :: Integer -> Integer -> Integer ->
  Float, which takes three integers and returns their average as a floating
  point number. To convert integers to floating point numbers, use the
  standard function fromInteger.-}
avgThree :: Integer -> Integer -> Integer -> Float
avgThree x y z = fromInteger (x + y + z) / 3

{-7. Use avgThree to write a function that takes three integers and returns
  the number of them that are above the average value of the three-}
numAboveAvg :: Integer -> Integer -> Integer -> Integer
numAboveAvg x y z =
  let avg = avgThree x y z
  in toInteger . length $ filter ((> avg) . fromInteger) [x, y, z]
