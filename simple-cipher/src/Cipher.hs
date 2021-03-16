module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Control.Monad (replicateM)
import Debug.Trace
import System.Random

letters = ['a' .. 'z']

caesar :: (Int -> Int -> Int) -> String -> String -> String
caesar f key text =
  let txt = zip (key ++ repeat (head key)) text
   in foldr (\(i, ch) acc -> encode i ch ++ acc) "" txt
  where
    index ch = length $ takeWhile (/= ch) letters
    encode k ch =
      let ki = index k
          chi = index ch
          i = f chi ki `mod` 26
       in [letters !! i]

caesarDecode :: String -> String -> String
caesarDecode = caesar (-)

caesarEncode :: String -> String -> String
caesarEncode = caesar (+)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  n <- replicateM 100 randomCh
  return (n, caesarEncode n text)
  where
    randomCh :: IO Char
    randomCh = fmap (letters !!) (randomRIO (0, 25))
