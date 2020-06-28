module DND
  ( Character (..),
    ability,
    modifier,
    character,
  )
where

import Data.List (sort)
import Test.QuickCheck (Gen, choose, vectorOf)

data Character
  = Character
      { strength :: Int,
        dexterity :: Int,
        constitution :: Int,
        intelligence :: Int,
        wisdom :: Int,
        charisma :: Int,
        hitpoints :: Int
      }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier constit = (constit - 10) `div` 2

ability :: Gen Int
ability = do
  v <- vectorOf 4 $ choose (1, 6)
  return $ (sum . tail . sort) v

character :: Gen Character
character = do
  s <- ability
  d <- ability
  c <- ability
  i <- ability
  w <- ability
  ca <- ability
  return $ Character s d c i w ca (10 + (modifier c))
