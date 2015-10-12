module Homework3.Golf
       ( skips
       ) where

skips :: [a] -> [[a]]
skips l = [e n l | n <- [1..length l]]
  where e n l = case drop (n-1) l of
          [] -> []
          (x:xs) -> x : e n xs
