module Homework1
       ( toDigits
       , toDigitsRev
       ) where

import Data.Char

toDigits :: Int -> [Int]
toDigits x = map digitToInt (show x) 

toDigitsRev :: Int -> [Int]
toDigitsRev = reverse . toDigits
