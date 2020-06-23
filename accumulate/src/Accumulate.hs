module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f xs = fmap f xs
