module Week11 where



import System.Random 
import Text.Read 


{-1. Run the following code then transform it to use monadic style instead of
do-notation:
import System.Random
randomChars :: RandomGen g => g -> String
randomChars gen = randomRs ('a', 'z') gen
main = do
g <- newStdGen
let cs = randomChars g
tenChars = take 10 cs
putStrLn tenChars-}


randomChars :: RandomGen g => g -> String 
randomChars = randomRs ('a', 'z') 

week11 :: IO () 
week11 = do 
    g <- newStdGen
    (putStrLn . take 10) $ randomChars g 


{-2. Given a Coin datatype, generate an infinite sequence of random coin
tosses:
data Coin = Head | Tail deriving (Show, Eq)
floatToCoin :: Float -> Coin
-- less than 0.5, heads, otherwise tails
randomCoins :: RandomGen g => g -> [Coin]
-- get an infinite list of random floats and turn that into a
-- list of coins
(Hint: when you test randomCoins in the interpreter, type Ctrl-C Ctrl-C
to stop its evaluation.)-}

data Coin = Head | Tail deriving (Show, Eq)

floatCoin :: Float -> Coin 
floatCoin f 
    | f < 0.5   = Head 
    | otherwise = Tail


randomCoins :: RandomGen g => g -> [Coin]
randomCoins gen = floatCoin <$> randomRs (0, 1) gen 


{-3. Create a function which calculates the percentage (as a whole number) of
a list of Coins which are Heads. The function should return a pair of the
number of coins inspected and the percentage of Heads:
pcHeads :: [Coin] -> (Int, Int)
(Hint: use the round and fromIntegral functions to convert from real
number to whole numbers and vice versa.)-}

pcHeads :: [Coin] -> (Int, Int)
pcHeads = foldl go (0,0)
    where 
        go (total, heads) Head = (total+1, heads + 1)
        go (total, heads) Tail = (total+1, heads) 


{-4. Create a function called display which takes the output from pcHeads
and prints a string to the terminal. For example,
> display (10, 50)
"Looked at 10 coins and 50% of them were heads."-}

display :: (Int, Int) -> IO () 
display = print 

{-5. Create a main function which reads in a number from the command line,
say n, then generates a list of n random coin tosses and prints out a message to the 
user containing the percentage of the random coin tosses which
were heads. To read command line arguments, import System.Environment
and use the getArgs functions. getArgs returns a list of strings, so you
need to convert the first one into a whole number - you can turn a string,
s, into an Int like so:
main = do
.....
let i = (read s) :: Int
Test your code in the interpreter using the special syntax to supply arguments to the main method,
 e.g. :main 100. Write your main function
using a do block first of all, then transform it to use monadic and/or
applicative style.-}

