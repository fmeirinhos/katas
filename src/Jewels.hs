----- 1st iteration: max-min with lists
splitAt' n xs = let (a, b) = splitAt (n+1) xs in [init a, b]

leastBribes [] = 0
leastBribes xs = minimum $ map (\i -> xs !! i + maximum (map leastBribes (splitAt' i xs))) [0..length xs-1]
-----


----- 2nd iteration: max-min with folds
leastBribes [] = 0
leastBribes xs = foldl f (sum xs) [1.. length xs] where
  f a i = let (l,r) = splitAt i xs in min a (last l + max (leastBribes (init l)) (leastBribes r))
-----


----- 3rd iteration: max-min with memoization (with dictionaries) with State Monads
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

-- Memoization
getOrUpdate :: (Ord k) => k -> State (Map.Map k v) v -> State (Map.Map k v) v
getOrUpdate k ifEmptyState = do
  maybeVal <- gets (Map.lookup k)
  case maybeVal of
    Just v -> return v
    Nothing -> do
      ifEmpty <- ifEmptyState
      modify (Map.insert k ifEmpty)
      return ifEmpty

splitAt' n xs = let (a, b) = splitAt (n+1) xs in (last a, init a, b)

leastBribes' [] = return 0
leastBribes' xs = do
  bs <- mapM f [0.. length xs - 1]
  return $ minimum bs where
  
  f i = let (b, l, r) = splitAt' i xs in do
    l' <- getOrUpdate l (leastBribes' l)
    r' <- getOrUpdate r (leastBribes' r)
    return $ b + max l' r'

leastBribes xs = evalState (leastBribes' xs) Map.empty
-----


----- 4th iteration: max-min with pure memoization (with dictionaries), strict evaluation and better criteria
import qualified Data.Map.Strict as Map

leastBribes xs = snd $ lb Map.empty xs where
  lb m [] = (m, 0)
  lb m xs = foldr f (m, (sum xs)) [1.. length xs] where
    f i (m, a) = (m', if a <= v then a else min a (v + max l' r')) where
      (v, l, r) = let (r, s) = splitAt i xs in (last r, init r, s)
      (m', l', r') = case Map.lookup l m of
        Just l_ -> case Map.lookup r m of
          Just r_ -> (m, l_, r_)
          _       -> let (n, r_) = lb m r in (Map.insert r r_ n, l_, r_)
        _       -> let {(n, l_) = lb m l; n' = Map.insert l l_ n} in case Map.lookup r n' of
          Just r_ -> (n', l_, r_)
          _       -> let (n'', r_) = lb n' r in (Map.insert r r_ n'', l_, r_)
-----


----- 5th iteration: pure memoization (with nested lists) and improved algorithm
leastBribes xs = lbMemo !! 0 !! length xs where
  lbMemo = [[lb n m | m <- [0..]] | n <- [0..]] where
    lb a b
      | a >= b    = 0
      | otherwise = minimum $ map (\i -> xs !! i + max (lbMemo !! a !! i) (lbMemo !! (i+1) !! b)) [a..b-1]
-----