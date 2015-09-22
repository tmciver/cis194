module Homework1
       ( toDigits
       , toDigitsRev
       , doubleEveryOther
       , sumDigits
       , validate
       , hanoi
       ) where

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits (div x 10) ++ [mod x 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = if odd (length xs) then
                        doubleHelper False xs
                      else
                        doubleHelper True xs
   where
   doubleHelper p xs2 = case xs2 of
     (y:ys) -> let h = if p then 2*y else y in
       h:doubleHelper (not p) ys
     [] -> []

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (\z n -> z + foldl (+) 0 (toDigits n))
               0
               xs

validate :: Integer -> Bool
validate x = mod y 10 == 0
  where
    y = let f = sumDigits . doubleEveryOther . toDigits in
      f x

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDiscs p1 p2 p3 = if numDiscs == 1 then
                            [(p1, p2)]
                          else
                            hanoi (numDiscs-1) p1 p3 p2 ++
                            hanoi 1 p1 p2 p3 ++
                            hanoi (numDiscs-1) p3 p2 p1
