module Codewars.Kata.Permutations (permutations) where

import Data.List (nub, delete)

permutations :: String -> [String]
permutations [] = [[]]
permutations xs = concatMap (\x -> map (x:) $ permutations $ delete x xs) (nub xs)