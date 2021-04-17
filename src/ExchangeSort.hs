module ExchangeSort (exchangeSort) where

import Data.List (elemIndices, sort, intersect)

deleteElems idx xs = foldl (flip deleteElem) xs (reverse $ sort idx) where
  deleteElem idx xs = let (lft, _:rgt) = splitAt idx xs in lft ++ rgt

exchangeSort :: [Int] -> Int
exchangeSort y = head $ filter ((==[]) . snd) (iterate (forceSwap . naturalSwap) (0, y)) where
  naturalSwap (i, x') = (i + m, deleteElems (take m idx1 ++ take m idx2) x) where
    (x, s) =  unzip [(a,b) | (a,b) <- zip x' (sort x'), a /= b]
    idx1 = elemIndices (head x) s `intersect` elemIndices (head s) x
    idx2 = elemIndices (head x) x `intersect` elemIndices (head s) s
    m = min (length idx1) (length idx2)
  forceSwap (i, [])  = (i, [])
  forceSwap (i, x)   = (i + 1, let j = head (elemIndices (minimum x) x) in [x !! j] ++ take (j - 1) (tail x) ++ [head x] ++ drop (j + 1) x)