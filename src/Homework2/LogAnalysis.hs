{-# OPTIONS_GHC -Wall #-}
module Homework2.LogAnalysis
       ( parseMessage
       ) where

import Homework2.Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E":severity:timestamp:message) -> LogMessage (Error (read severity)) (read timestamp) (unwords message)
  ("I":timestamp:message) -> LogMessage Info (read timestamp) (unwords message)
  ("W":timestamp:message) -> LogMessage Warning (read timestamp) (unwords message)
  _ -> Unknown s
