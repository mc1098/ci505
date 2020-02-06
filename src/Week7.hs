module Week7 where

import System.IO
import Log
import Data.Char (isDigit)
import Text.Read
import Data.List

{-1.1 Exercise 1
  The first thing we need to do is to read the log file. Define a function
  readLogFile :: String -> IO [String]
  that takes the path to a log file, reads its contents and returns a list of strings
  in which each element contains one line from the log file. Test your work with
  the function testReadLogFile, provided in Log.hs. testReadLogFile takes
  your readLogFile function, a String containing the path to the log file, and
  simply prints the contents of the list of strings. So, it's output should match
  the original file. Use sample.log for testing (you could also use error.log but
  it is quite a large file).-}

readLogFile :: String -> IO [String]
readLogFile = fmap lines . readFile

{-1.2 Exercise 2
  The next step is figuring out how to parse an individual message. However,
  perhaps the file is even more corrupted than we thought: maybe individual
  lines are garbled. So, we can't be sure that a line from the input will be a valid
  LogMessage. Thus, we define a type (included in the provided Log.hs) to allow
  for the possibility of failure:
  type MaybeLogMessage = ValidLM LogMessage | InvalidLM String deriving
  (Show, Eq)
  As you can see, a MaybeLogMessage either contains a proper LogMessage or
  just an unformatted string. Now, you can define a function
  parseMessage:: String -> MaybeLogMessage
  which parses an individual line from the logfile. For example,
  parseMessage "E 2 562 helphelp"
  == ValidLM (LogMessage (Error 2) 562 "helphelp")
  parseMessage "I 29 lalala"
  == ValidLM(LogMessage Info 29 "lalala")
  parseMessage "This is not in the right format"
  == InvalidLM "This is not in the right format"
  Use the read function to convert Strings to Ints. Test your work with
  the function testParseMessage, which takes as arguments your readLogFile
  function, the path to a log file, and your parseMessage function.
-}

parseMessage :: String -> MaybeLogMessage
parseMessage s = parseMessage' $ words s
  where
    parseMessage' [] = InvalidLM []
    parseMessage' (x:xs) = case x of
      "I" -> parseGenMessage Info xs
      "E" -> parseErrorMessage xs
      "W" -> parseGenMessage Warning xs
      _ -> InvalidLM $ unwords (x:xs)
      where
        parseGenMessage _ [] = InvalidLM []
        parseGenMessage mt (x:xs) = case readMaybe x of
          Nothing -> InvalidLM $ unwords (x:xs)
          Just y  -> ValidLM $ LogMessage mt y $ unwords xs

        parseErrorMessage [] = InvalidLM []
        parseErrorMessage (x:xs)
          | length xs >= 2 = case readMaybe x of
            Nothing -> InvalidLM $ unwords ("E":x:xs)
            Just y  -> case readMaybe $ head xs of
              Nothing -> InvalidLM $ unwords ("E":x:xs)
              Just z  ->  ValidLM $ LogMessage (Error y) z $ unwords $ tail xs
          | otherwise = InvalidLM $  show $ unwords ("E":x:xs)


{-1.3 Exercise 3
  It isn't terribly hard to make parseMessage work over all the lines in a file.
  But, doing so would produce a list of MaybeLogMessages, where we really just
  want a list of LogMessages for further processing - let's throw out the invalid
  messages. Write a function
  validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
  that throws out invalid messages.-}
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly = foldl go []
  where
    go acc (InvalidLM _) = acc
    go acc (ValidLM x)   = x:acc

{-1.4 Exercise 4
  Now, we can put these pieces together to define
  parse :: String -> IO [LogMessage]
  which parses an entire log file at once and returns its contents as a list of
  LogMessages. To test your function, use the testParse function provided in
  the Log module, giving it as arguments your parse function, the number of
  messages to parse, and the log file to parse from (which should also be in the
  same folder as your assignment). For example, after loading your assignment
  into GHCi, type something like this at the prompt:
  testParse parse 10 "error.log"
-}

parse :: String -> IO [LogMessage]
parse path = validMessagesOnly . map parseMessage <$> readLogFile path


{-2.1 Exercise 5
  Any sorting function is going to need to compare two LogMessages to see which
  one should come first. But, since we've just created the LogMessage type, there
  is no way for the computer to know how to compare two of them. We must
  write a comparison function! In general, comparing two items for ordering can
  yield one of three results: less-than, equal-to, or greater-than. Haskell codifies
  this idea as a datatype
  data Ordering = LT | EQ | GT
  Ordering is part of the Prelude (the set of things automatically included),
  so its definition doesn't appear in Log.hs nor should it appear in your code.
  Define a function
  compareMsgs :: LogMessage -> LogMessage -> Ordering
  that compares two LogMessages based on their timestamps. Here are some
  examples:
  compareMsgs (LogMessage Warning 153 "Not a speck of light is showing, so
  the danger must be
  growing...") (LogMessage Info 208 "the Weighted Companion Cube cannot
  talk")
  == LT
  compareMsgs (LogMessage (Error 101) 2001 "My God! It's full of stars!")
  (LogMessage Info 2001
  "Daisy, Daisy, give me your answer do.")
  == EQ-}

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 `compare` t2

{-2.2 Exercise 6
  Now that you have said how to compare messages, you can sort the list. Write
  a function
  sortMessages :: [LogMessage] -> [LogMessage]
  that sorts the list of messages. Do not write out a full sorting algorithm!
  Instead, poke around in the Data.List module looking for a function that will
  help you.-}
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

{-3.1 Exercise 7
  Now that we can sort the log messages, the only thing left to do is extract the
  relevant information. We have decided that "relevant" means "errors with a
  severity of at least 50". Write a function
  whatWentWrong :: [LogMessage] -> [(TimeStamp, String)]
  which takes an unsorted list of LogMessages and returns a list of the messages
  corresponding to any errors with a severity of 50 or greater, sorted by
  timestamp. (Of course, you can use your functions from the previous exercises
  to do the sorting.) For example, suppose our logfile looked like this:
  I 6 Completed armadillo processing
  I 1 Nothing to report
  E 99 10 Flange failed!
  I 4 Everything normal
  I 11 Initiating self-destruct sequence
  E 70 3 Way too many pickles
  E 65 8 Bad pickle-flange interaction detected
  W 5 Flange is due for a check-up
  I 7 Out for lunch, back in two timesteps
  E 20 2 Too many pickles
  I 9 Back from lunch
  This file is provided as sample.log. There are four errors, three of which
  have a severity of greater than 50. The output of whatWentWrong on sample.log
  ought to be
  [(3,"Way too many pickles"),(8,"Bad pickle-flange interaction
  detected"),(10,"Flange failed!")]
  You can test your whatWentWrong function with testWhatWentWrong, which
  is also provided by the Log module. You should provide testWhatWentWrong
  with your parse function, your whatWentWrong function, and the name of the
  logfile to parse.
-}

whatWentWrong :: [LogMessage] -> [(TimeStamp, String)]
whatWentWrong = foldl' go [] . sortMessages
  where
    go acc (LogMessage (Error x) ts s)
      | x > 50    = (ts, s):acc
      | otherwise = acc
    go acc _ = acc
    

{-3.2 Exercise 8
  Having read log files, parsed individual messages, sorted them and detected
  messages with certain properties, we now need to present the results to our
  boss. So, we will write the results of our analysis to a file. This file will present
  a sorted list of the most severe incidents, showing just the time at which an
  incident occurred and the error message. Define the function
  processLogFile :: String -> String -> IO ()
  which does the following:
  1. read the log file at the path given by the first String argument,
  2. parse the log file into a list of messages,
  3. transform that list into a sorted list of error messages with a severity of
  at least 50,
  4. write this sorted list to a file whose name is given by the second String argument. 
  Each row in the file should have the form [timestamp] message.
  For example,
  [3] Way too many pickles
  [8] Bad pickle-flange interaction detected
  [10] Flange failed!
-}    

processLogFile :: String -> String -> IO ()
processLogFile src dst = do 
  msgs <- parse src 
  writeErrorsToFile dst $ formatErrors $ whatWentWrong msgs
  where 
    formatErrors :: [(TimeStamp, String)] -> [String] 
    formatErrors = foldl' (\acc (ts, s) -> ('[' : show ts ++ "] " ++ s):acc) [] 
    writeErrorsToFile :: String -> [String] -> IO ()
    writeErrorsToFile dst = writeFile dst . unlines