module Homework1
       ( toDigits
       , toDigitsRev
       , doubleEveryOther
       ) where

import Data.Char

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

toDigitsRev :: Int -> [Int]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther xs = if odd (length xs) then
                        doubleHelper False xs
                      else
                        doubleHelper True xs
   where
   doubleHelper p xs2 = case xs2 of
     (y:ys) -> let h = if p then 2*y else y in
       h:doubleHelper (not p) ys
     [] -> []
