
module Week1 where

{- Intro: test is a function that returns the String "Hello World!". 
  Loading this file in ghci and invoking this will show the String value!  -}
test :: String
test = "Hello World!"

{- 1. Edit Exercises.hs to write a function square which returns the square of a number.-}
square :: Int ->  Int
square x = x * x

{- Use square to write a function sumsquare :: Int -> Int -> Int which
  returns the sum of the squares of its two arguments -}
sumsquare :: Int -> Int -> Int
sumsquare x y = square x + square y

{-2. Write a recursive function memb which takes two arguments, a list and
  an element to search for in that list. Your function should use pattern
  matching to return True if the second input is a member of the list which
  is the first input. The base case, which deals with what happens if an
  empty list is supplied, should return False. The inductive case, which
  deals with non-empty lists, should return True if the element to be found
  matches the head of the list. If not, the function should return the result
  of calling itself on the tail of the list.-}
memb :: Eq a => [a] -> a -> Bool
memb [] _ = False
memb (x:xs) y
  | x == y    = True
  | otherwise = memb xs y

{-3. Write a (recursive) function length' :: [a] -> Int which calculates
  the length of a list. Think about the base case of the empty list and the
  case when the list contains elements.
-}
length' :: [a] -> Int
length' xs = go xs 0
  where
    go [] n = n
    go (_:xs) n = go xs (1+n)

{-4. Write a function drop' :: Int -> [a] -> [a], where drop' n xs returns xs
 with its first n elements removed.-}
drop' :: Int -> [a] -> [a]
drop' n xs 
  | n <= 1    = xs            --check for minus or zero 
  | otherwise = go n xs       
  where 
    go _ [] = [] 
    go 1 (_:xs) = xs 
    go n (_:xs) = go (n-1) xs

{-5. Write a function take' :: Int -> [a] -> [a], where take' n xs returns 
  the first n elements of xs as a list (if xs contains less than n elements,
  your function should return all of xs).-}
take' :: Int -> [a] -> [a] 
take' n xs 
  | n <= 1 = [] 
  | otherwise = go n xs 
  where 
    go _ []     = [] 
    go 1 (x:_)  = [x] 
    go n (x:xs) = x: go (n-1) xs  --take backwards maintaining order and using O(1) (:) over O(n) (++) op