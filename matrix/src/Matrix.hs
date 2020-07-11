module Matrix
  ( Matrix,
    cols,
    column,
    flatten,
    fromList,
    fromString,
    reshape,
    row,
    rows,
    shape,
    transpose,
  )
where

import Control.Monad (join)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix xs)
  | null xs || null (xs V.! 0) = 0
  | otherwise = V.length (xs V.! 0)

column :: Int -> Matrix a -> Vector a
column x (Matrix xs) = foldr (\r acc -> V.cons (r V.! (x -1)) acc) V.empty xs

flatten :: Matrix a -> Vector a
flatten (Matrix xs) = join xs

fromList :: [[a]] -> Matrix a
fromList xs = Matrix $ V.fromList $ V.fromList <$> xs

fromString :: Read a => String -> Matrix a
fromString xs = fromList $ (fmap . fmap) read $ nums 
  where
    nums = fmap words (lines xs)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_, c) matrix =
  Matrix
    $ V.takeWhile (not . V.null) . V.unfoldr (Just . V.splitAt c)
    $ flatten matrix

row :: Int -> Matrix a -> Vector a
row x (Matrix xs) = xs V.! (x -1)

rows :: Matrix a -> Int
rows (Matrix xs)
  | null xs = 0
  | otherwise = V.length xs

shape :: Matrix a -> (Int, Int)
shape matrix = ((rows matrix), (cols matrix))

transpose :: Matrix a -> Matrix a
transpose matrix = Matrix $ V.fromList $ (\i -> column i matrix) <$> [1 .. (cols matrix)]

