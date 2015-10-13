module Homework3.Golf
       ( skips
       , localMaxima
       ) where

skips :: [a] -> [[a]]
skips l = [e n l | n <- [1..length l]]
  where e n l = case drop (n-1) l of
          [] -> []
          (x:xs) -> x : e n xs

localMaxima :: [Integer] -> [Integer]
localMaxima = map (head . tail) . filter h . g
  where g xs | length xs < 3 = []
        g xs@(_:xss) = take 3 xs : g xss
        h (a:b:c:_) = b > a && b > c
