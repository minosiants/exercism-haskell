module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n
  | ops n 4 = reverse operations
  | otherwise = operations
  where
    operations = (filter (ops n) [0, 1, 2, 3]) >>= toOp
    -- testBit should be used instead
    ops _n op = ((shiftR _n op) .&. 1) == 1
    toOp _n =
      case _n of
        0 -> ["wink"]
        1 -> ["double blink"]
        2 -> ["close your eyes"]
        3 -> ["jump"]
        _ -> []
