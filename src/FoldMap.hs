module Foldmap where

import Data.Foldable (foldMap, Foldable)
import Data.Monoid (mappend)

myToList :: Foldable t => t a -> [a]
myToList = foldMap return

newtype Minimum a = Minimum {getMinimum :: Maybe a}

instance Ord a => Monoid (Minimum a) where
  Minimum (Just x) `mappend` Minimum Nothing = Minimum (Just x)
  Minimum (Just x) `mappend` Minimum (Just y) = Minimum . Just $ min x y
  Minimum Nothing `mappend` x = x
  mempty = Minimum Nothing

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum = getMinimum . foldMap (Minimum . Just)

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f b = foldr f b . myToList
