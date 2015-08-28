module LogAnalysis where

import Log
import Data.List hiding (insert)

parseMessage :: String -> LogMessage
parseMessage str = case messageType of
                   "I" -> LogMessage Info second second'
                   "W" -> LogMessage Warning second second'
                   "E" -> LogMessage (Error second) third third'
                   _ -> Unknown str
  where ws = words str
        messageType = head ws
        second = read $ ws !! 1
        third = read $ ws !! 2
        second' = unwords $ drop 2 ws
        third' = unwords $ drop 3 ws

parse :: String -> [LogMessage]
parse = map parseMessage . lines

instance Ord LogMessage where
  (LogMessage _ ts1 _) `compare` (LogMessage _ ts2 _) = ts1 `compare` ts2
  (LogMessage _ _ _) `compare` (Unknown _) = GT
  (Unknown _)  `compare` (LogMessage _ _ _) = LT
  (Unknown str1) `compare` (Unknown str2) = str1 `compare` str2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message (Node left message' right)
  | message > message' = Node left message' (insert message right)
  | message < message' = Node (insert message left) message' right
  | otherwise = Node left message right

build :: [LogMessage] -> MessageTree
build = foldl' (\tree message -> insert message tree) emptyTree
  where emptyTree = Leaf


main = testBuild

testBuild = do
  messages <- testParse parse 10 "error.log"
  let tree1 = build messages
  print tree1

testInsert = do
  let tree1 = Leaf
      message1 = parseMessage "I 29 la la la"
      message2 = parseMessage "E 2 562 help help"
      message3 = parseMessage "This is not in the right format"
      message4 = parseMessage "I 29 yo yo yo"
      tree2 = insert message1 tree1
      tree3 = insert message2 tree2
      tree4 = insert message4 tree3
  print tree2
  print tree3
  print tree4

testOrd = do
  let message1 = parseMessage "E 2 562 help help"
      message2 = parseMessage "I 29 la la la"
      message3 = parseMessage "This is not in the right format"
  print message1
  print message2
  print message3
  print $ message1 > message2
  print $ message1 > message3
  print $ message3 > message2

  -- print $ parseMessage "This is not in the right format"
  -- l <- testParse parse 10 "error.log"
  -- print $ l !! 0
