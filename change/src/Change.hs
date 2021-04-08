module Change (findFewestCoins) where

import Data.List
import Data.Function
import Data.Maybe
import Control.Monad

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0  = Nothing
  | target == 0 = Just []
  | otherwise   = selectShortest $ catMaybes $ f <$> filter (<=target) coins
  where f c = let ns = target`div`c in msum $ g c <$> [ns,ns-1..1]
        g c n = (replicate (fromIntegral n) c ++) <$> findFewestCoins (target-c*n) coins

selectShortest :: [[a]] -> Maybe [a]
selectShortest [] = Nothing
selectShortest xs = Just $ minimumBy (compare`on`length) xs
