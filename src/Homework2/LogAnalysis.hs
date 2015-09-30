{-# OPTIONS_GHC -Wall #-}
module Homework2.LogAnalysis
       ( parseMessage
       , insert
       , build
       , inOrder
       ) where

import Homework2.Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E":severity:timestamp:message) -> LogMessage (Error (read severity)) (read timestamp) (unwords message)
  ("I":timestamp:message) -> LogMessage Info (read timestamp) (unwords message)
  ("W":timestamp:message) -> LogMessage Warning (read timestamp) (unwords message)
  _ -> Unknown s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t -- don't insert Unknown log messages
insert lm@(LogMessage _ ts _) t = case t of
  Leaf -> Node Leaf lm Leaf
  Node leftTree alm@(LogMessage _ ats _) rightTree -> if ts < ats then
                                                        Node (insert lm leftTree) alm rightTree
                                                      else if ts > ats then
                                                             Node leftTree alm (insert lm rightTree)
                                                           else
                                                             t

build :: [LogMessage] -> MessageTree
build = foldl (\t m -> insert m t) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree lm rightTree) = (inOrder leftTree) ++ [lm] ++ (inOrder rightTree)
