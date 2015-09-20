module Homework1
       ( toDigits
       , toDigitsRev
       ) where

import Data.Char

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

toDigitsRev :: Int -> [Int]
toDigitsRev = reverse . toDigits
