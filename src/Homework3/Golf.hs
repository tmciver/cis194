module Homework3.Golf
       ( skips
       ) where

skips :: [a] -> [[a]]
skips l = [every n l | n <- [1..(length l)]]
  where every _ [] = []
        every n l = case drop (n-1) l of
          [] -> []
          (x:xs) -> x : every n xs
