{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-
data MessageType = Info
                 | Warning
                 | Error Int
                 deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
                deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
                 deriving (Show, Eq)
-}

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  ("I" : timestamp : xs) -> LogMessage Info (read timestamp) (unwords xs)
  ("W" : timestamp : xs) -> LogMessage Warning (read timestamp) (unwords xs)
  ("E" : level : timestamp : xs) -> LogMessage (Error $ read level) (read timestamp) (unwords xs)
  msglist -> Unknown (unwords msglist)

parse :: String -> [LogMessage]
-- map only works on lists
-- fmap, or its infix `<$>`, works on functors (more general case)
parse inputString = map parseMessage (lines inputString)

exercise1 :: IO ()
exercise1 = do
  putStr "\n"
  putStrLn "Exercise 1"
  print $ parseMessage "E 2 562 help help"
  print $ parseMessage "I 29 la la la"
  print $ parseMessage "This is not in the right format"
  -- lol, monad sighted. wtf is that even
  --print =<< testParse parse 5 "error.log"

insert :: LogMessage -> MessageTree -> MessageTree
insert newMsg tree = case newMsg of
  Unknown _ -> tree
  LogMessage _ ts _ -> case tree of
    Leaf -> Node Leaf newMsg Leaf
    Node left incMsg@(LogMessage _ ts2 _) right
      | ts <= ts2 -> Node (insert newMsg left) incMsg right
      | ts > ts2 -> Node left incMsg (insert newMsg right)
    Node {} -> tree

exercise2 :: IO ()
exercise2 = do
  putStr "\n"
  putStrLn "Exercise 2"
  putStrLn $ "'Unknown' type returns a Leaf... " ++ show (insert (Unknown "bleh") Leaf == Leaf)
  putStrLn $ "'LogMessage' type returns a MessageTree... " ++ show (insert (parseMessage "E 2 562 help help") Leaf == Node Leaf (LogMessage (Error 2) 562 "help help") Leaf)

build :: [LogMessage] -> MessageTree
build msgs = case msgs of
  [] -> Leaf
  [x] -> insert x Leaf
  (x:xs) -> insert x (build xs)

exercise3 :: IO ()
exercise3 = do
  putStr "\n"
  putStrLn "Exercise 3 -- no tests =P"

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
  Leaf -> []
  Node left msg right -> inOrder left ++ [msg] ++ inOrder right

exercise4 :: IO ()
exercise4 = do
  putStr "\n"
  putStrLn "Exercise 4"

  let text = unlines [ "I 6 Completed armadillo processing"
                     , "I 1 Nothing to report"
                     , "E 99 10 Flange failed!"
                     , "I 4 Everything normal"
                     , "I 11 Initiating self-destruct sequence"
                     , "E 70 3 Way too many pickles"
                     , "E 65 8 Bad pickle-flange interaction detected"
                     , "W 5 Flange is due for a check-up"
                     , "I 7 Out for lunch, back in two time steps"
                     , "E 20 2 Too many pickles"
                     , "I 9 Back from lunch"
                     ]

  let ordered = parse $ unlines [ "I 1 Nothing to report"
                                , "E 20 2 Too many pickles"
                                , "E 70 3 Way too many pickles"
                                , "I 4 Everything normal"
                                , "W 5 Flange is due for a check-up"
                                , "I 6 Completed armadillo processing"
                                , "I 7 Out for lunch, back in two time steps"
                                , "E 65 8 Bad pickle-flange interaction detected"
                                , "I 9 Back from lunch"
                                , "E 99 10 Flange failed!"
                                , "I 11 Initiating self-destruct sequence"
                                ]

  putStrLn $ "in-order parsing of tree: order is as expected..." ++ show (inOrder (build $ parse text) == ordered)

--whatWentWrong :: [LogMessage] -> [String]

exercise5 :: IO ()
exercise5 = do
  putStr "\n"
  putStrLn "Exercise 5"

  let text = unlines [ "I 6 Completed armadillo processing"
                     , "I 1 Nothing to report"
                     , "E 99 10 Flange failed!"
                     , "I 4 Everything normal"
                     , "I 11 Initiating self-destruct sequence"
                     , "E 70 3 Way too many pickles"
                     , "E 65 8 Bad pickle-flange interaction detected"
                     , "W 5 Flange is due for a check-up"
                     , "I 7 Out for lunch, back in two time steps"
                     , "E 20 2 Too many pickles"
                     , "I 9 Back from lunch"
                     ]

  let expected = [ "Way too many pickles"
                 , "Bad pickle-flange interaction detected"
                 , "Flange failed!"
                 ]

  putStrLn $ "we can filter for errors with severity 50... " ++ show False

main :: IO ()
main = do
  exercise1
  exercise2
  exercise3
  exercise4
  exercise5
