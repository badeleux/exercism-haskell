module Hamming (distance) where

import Data.Maybe

distance :: String -> String -> Maybe Int
distance xs ys 
  | length xs == length ys = let l = [fromEnum $ x /= y | (x,y) <- zip xs ys]
                             in Just . sum $ l
  | otherwise = Nothing


