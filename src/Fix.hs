module Fixit where
import Prelude hiding (reverse, foldr)

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' f a@(_:_) = last a : f (init a)
reverse' f [] = []

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' f g z [] = z
foldr' f g z (x:xs) = x `g` f g z xs