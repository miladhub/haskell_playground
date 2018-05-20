{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage x =
  case ws of "E" : errno : ts : s -> LogMessage (Error (read errno)) (read ts) (unwords s)
             "I" : ts : s -> LogMessage Info (read ts) (unwords s)
             "W" : ts : s -> LogMessage Warning (read ts) (unwords s)
             _ -> Unknown x
    where ws = words x

parse :: String -> [LogMessage]
parse s = fmap parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logmessage Leaf = Node Leaf logmessage Leaf
insert logmessage@(LogMessage msgtype1 ts1 msg1) (Node t1 (LogMessage msgtype2 ts2 msg2) t2)
	| ts1 < ts2 = (Node (insert logmessage t1) (LogMessage msgtype2 ts2 msg2) t2)
	| otherwise = (Node t1 (LogMessage msgtype2 ts2 msg2) (insert logmessage t2))
	
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (l : ls) = insert l (build ls)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 msg t2) = (inOrder t1) ++ [msg] ++ (inOrder t2)

{-
let l1 = LogMessage Info 2 "Foo"
let l2 = LogMessage (Error 40) 3 "Bar1"
let l3 = LogMessage (Error 55) 4 "Bar2"
let l4 = LogMessage (Error 60) 5 "Baz"
let l5 = Unknown "FooBar"
let t = build [l1, l2, l3, l4, l5]
inOrder $ build [l2, l4, l3, l1, l5]
whatWentWrong  [l2, l4, l1, l3, l5]
testWhatWentWrong parse whatWentWrong "sample.log" 
-}

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = fmap getmsg (filter critical orderedmsgs)
	where orderedmsgs = (inOrder $ build msgs)
	      getmsg (LogMessage _ _ msg) = msg
	      critical (LogMessage (Error level) _ _) = (level > 50)
	      critical _ = False