module Stream where

import Control.Arrow
import Control.Applicative

import Stream.Internal

-- Defined in Stream.Internal:
--     data Stream a = a :> Stream a
--     infixr :>

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (a:>b) = a

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (a:>b) = b


-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS a = a :> repeatS a

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS a = foldr (:>) (cycleS a) a

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS x = x :> fromS (x+1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = x :> fromStepS (x+s) s

-- }}}