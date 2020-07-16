module Matrix (saddlePoints) where

import Control.Monad (join)
import Data.Array (Array)
import qualified Data.Array as A
import Data.List (elemIndices)
import qualified Data.List as L

column :: Int -> [[a]] -> [a]
column i = foldr (\xs acc -> (xs !! i) : acc) []

rows :: Array (Int, Int) e -> [[e]]
rows arr = (fmap . fmap) snd $ L.groupBy (\((i, _), _) ((i', _), _) -> i == i') $ A.assocs arr

maxInRow :: Ord e => [[e]] -> [[(Int, e)]]
maxInRow =
  fmap
    ( \e ->
        let max' = L.maximum e
            index' = elemIndices max' e
         in (zipWith (\i _ -> (i, max')) index' index')
    )

minInColumn :: Ord e => Int -> [[e]] -> e
minInColumn n xs = L.minimum $ column n xs

saddlePoints :: Ord e => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix =
  let matrix' = rows matrix
      maxs' = maxInRow matrix'
      res =
        zipWith
          ( \row' xs ->
              foldr
                ( \(col, el) acc ->
                    if (minInColumn col matrix') < el then acc else (row', col + 1) : acc
                )
                []
                xs
          )
          [1 .. (length maxs')]
          maxs'
   in join res
