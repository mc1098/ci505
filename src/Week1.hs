
module Week1 where

test :: String
test = "Hello World!"

square :: Int ->  Int
square x = x * x

sumsquare :: Int -> Int -> Int
sumsquare x y = square x + square y

memb :: Eq a => [a] -> a -> Bool
memb [] _ = False
memb (x:xs) y
  | x == y    = True
  | otherwise = memb xs y

length' :: [a] -> Int
length' xs = go xs 0
  where
    go [] n = n
    go (_:xs) n = go xs (1+n)

drop' :: Int -> [a] -> [a]
drop' n xs 
  | n <= 1    = xs            --check for minus or zero 
  | otherwise = go n xs       
  where 
    go _ [] = [] 
    go 1 (_:xs) = xs 
    go n (_:xs) = go (n-1) xs


take' :: Int -> [a] -> [a] 
take' n xs 
  | n <= 1 = [] 
  | otherwise = go n xs 
  where 
    go _ []     = [] 
    go 1 (x:_)  = [x] 
    go n (x:xs) = x: go (n-1) xs  --take backwards maintaining order and using O(1) (:) over O(n) (++) op